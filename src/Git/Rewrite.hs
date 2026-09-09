-- | The rewrite engine: a fold over commits, oldest first, that carries a map
-- from original to rewritten ids.
module Git.Rewrite
  ( GitM
  , Transform (..)
  , pureTransform
  , withOriginal
  , Plan
  , Rewritten (..)
  , rewriteCommits
  , RewriteOptions (..)
  , defaultRewriteOptions
  , Report (..)
  , rewriteBranches
  ) where

import Control.Monad (foldM, unless, void, when, (>=>))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Git.Object (hashObject, isSigned, renderCommit, stripSignature)
import Git.Refs
import Git.Store
import Git.Types

type GitM = ReaderT Store IO

-- | Receives the commit's original id alongside the commit being rebuilt, so
-- per-commit choices (e.g. jitter) stay stable however earlier transforms or
-- parent rewrites change the bytes. Composes left to right with '<>';
-- 'mempty' is the identity rewrite.
newtype Transform = Transform {runTransform :: Oid -> Commit -> GitM Commit}

instance Semigroup Transform where
  Transform f <> Transform g = Transform (\o -> f o >=> g o)

instance Monoid Transform where
  mempty = Transform (const pure)

pureTransform :: (Commit -> Commit) -> Transform
pureTransform f = Transform (const (pure . f))

withOriginal :: (Oid -> Commit -> Commit) -> Transform
withOriginal f = Transform (\o -> pure . f o)

-- | Builds the transform after seeing the whole history to be rewritten
-- (original ids and commits, oldest first). In IO so a plan can set up state
-- that lives for the whole rewrite, such as memo tables. Plans form a monoid
-- via the function and IO instances.
type Plan = [(Oid, Commit)] -> IO Transform

data Rewritten = Rewritten
  { rwMapping    :: Map Oid Oid
  , rwOutOfOrder :: [Oid]
    -- ^ New commits whose committer date is now earlier than a parent's, when
    -- the original was not. Git doesn't mind, but date-sorted views (GitHub,
    -- @git log@ without @--topo-order@) will show them out of place.
  , rwPruned     :: [Oid]
    -- ^ Original commits dropped because the rewrite left them changing nothing.
  , rwUnsigned   :: [Oid]
    -- ^ Original commits whose signature was dropped because they changed.
  }

data Fold = Fold
  { fMapping   :: !(Map Oid Oid)
  , fNewInfo   :: !(Map Oid (Oid, Int64))
    -- ^ Tree and committer time of each new commit, for pruning and ordering checks.
  , fBackwards :: [Oid]
  , fPruned    :: [Oid]
  , fUnsigned  :: [Oid]
  }

-- | Commits must be in topological order, parents first. Parents are remapped
-- before the transform runs, so transforms see final parent ids (vanity mining
-- depends on this). Parents outside the list are kept as they are.
--
-- With pruning, a single-parent commit whose new tree equals its new parent's
-- is dropped (its id maps to the parent), unless it was already empty before.
rewriteCommits :: Store -> Bool -> Transform -> [(Oid, Commit)] -> IO Rewritten
rewriteCommits store prune t history = do
  f <- foldM step (Fold Map.empty Map.empty [] [] []) history
  pure (Rewritten (fMapping f) (reverse (fBackwards f)) (reverse (fPruned f)) (reverse (fUnsigned f)))
  where
    original = Map.fromList [(o, (cTree c, committerTime c)) | (o, c) <- history]
    committerTime = sigTime . cCommitter
    newerThan info time = any (maybe False ((> time) . snd) . (`Map.lookup` info))
    sameTreeAsParent info c = case cParents c of
      [p] | Just (tree, _) <- Map.lookup p info -> tree == cTree c
      _ -> False

    step f (oid, c) = do
      let remapped = c {cParents = map (\p -> Map.findWithDefault p p (fMapping f)) (cParents c)}
      transformed <- runReaderT (runTransform t oid remapped) store
      let c' = if transformed /= c then stripSignature transformed else transformed
          unsigned = if isSigned c && not (isSigned c') then oid : fUnsigned f else fUnsigned f
      case cParents c' of
        [parent]
          | prune && sameTreeAsParent (fNewInfo f) c' && not (sameTreeAsParent original c) ->
              pure f {fMapping = Map.insert oid parent (fMapping f), fPruned = oid : fPruned f}
        _ -> do
          let body = renderCommit c'
              new = hashObject ObjCommit body
              time = committerTime c'
              nowBackwards = newerThan (fNewInfo f) time (cParents c')
              wasBackwards = newerThan original (committerTime c) (cParents c)
          unless (new == oid) $ void (writeObject store ObjCommit body)
          pure
            f
              { fMapping = Map.insert oid new (fMapping f)
              , fNewInfo = Map.insert new (cTree c', time) (fNewInfo f)
              , fBackwards = if nowBackwards && not wasBackwards then new : fBackwards f else fBackwards f
              , fUnsigned = unsigned
              }

data RewriteOptions = RewriteOptions
  { roBranches   :: [ByteString]
    -- ^ Empty means every branch.
  , roDryRun     :: Bool
  , roPruneEmpty :: Bool
  }

defaultRewriteOptions :: RewriteOptions
defaultRewriteOptions = RewriteOptions [] False False

data Report = Report
  { rpCommits :: Int
  , rpChanged :: [(Oid, Oid)]
    -- ^ Oldest first. A pruned commit maps to its parent's new id.
  , rpOutOfOrder :: [Oid]
  , rpPruned :: [Oid]
  , rpUnsigned :: [Oid]
  , rpUpdates :: [RefUpdate]
  , rpSkippedTags :: [Ref]
  , rpBackup :: Maybe Int
    -- ^ Nothing for dry runs and rewrites that changed nothing.
  }

-- | Rewrite everything reachable from the chosen branches, then move those
-- branches and any lightweight tags that pointed into the rewritten history.
-- A dry run writes the new objects (unreferenced, so @git gc@ removes them)
-- but leaves every ref alone.
rewriteBranches :: Store -> RewriteOptions -> Plan -> IO Report
rewriteBranches store opts plan = do
  unless (roDryRun opts) (ensureClean store)
  branches <-
    if null (roBranches opts)
      then listRefs store ["refs/heads/"]
      else traverse (resolveBranch store) (roBranches opts)
  when (null branches) $ fail "no branches to rewrite"
  oids <- revList store (["--topo-order", "--reverse", "--end-of-options"] <> map (BC.unpack . refName) branches)
  history <- traverse (\o -> (o,) <$> readCommit store o) oids
  transform <- plan history
  Rewritten mapping backwards pruned unsigned <- rewriteCommits store (roPruneEmpty opts) transform history
  flushObjects store
  tags <- listRefs store ["refs/tags/"]
  let (updates, skipped) = planRefUpdates mapping (branches <> tags)
      changed = [(o, n) | o <- oids, let n = mapping Map.! o, n /= o]
  backup <-
    if roDryRun opts || null updates
      then pure Nothing
      else Just <$> applyRefUpdates store updates <* syncWorktree store
  pure (Report (length oids) changed backwards pruned unsigned updates skipped backup)
