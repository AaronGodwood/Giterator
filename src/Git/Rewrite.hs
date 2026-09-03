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
import Git.Object (hashObject, renderCommit)
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

-- | A transform chosen after seeing the whole history to be rewritten (original
-- ids and commits, oldest first). Plans form a monoid via the function instance.
type Plan = [(Oid, Commit)] -> Transform

data Rewritten = Rewritten
  { rwMapping    :: Map Oid Oid
  , rwOutOfOrder :: [Oid]
    -- ^ New commits whose committer date is now earlier than a parent's, when
    -- the original was not. Git doesn't mind, but date-sorted views (GitHub,
    -- @git log@ without @--topo-order@) will show them out of place.
  }

data Fold = Fold !(Map Oid Oid) !(Map Oid Int64) [Oid]

-- | Commits must be in topological order, parents first. Parents are remapped
-- before the transform runs, so transforms see final parent ids (vanity mining
-- depends on this). Parents outside the list are kept as they are.
rewriteCommits :: Store -> Transform -> [(Oid, Commit)] -> IO Rewritten
rewriteCommits store t history = do
  Fold mapping _ backwards <- foldM step (Fold Map.empty Map.empty []) history
  pure (Rewritten mapping (reverse backwards))
  where
    originalTimes = Map.fromList [(o, committerTime c) | (o, c) <- history]
    committerTime = sigTime . cCommitter
    newerThan times time = any (maybe False (> time) . (`Map.lookup` times))

    step (Fold mapping newTimes backwards) (oid, c) = do
      let remapped = c {cParents = map (\p -> Map.findWithDefault p p mapping) (cParents c)}
      c' <- runReaderT (runTransform t oid remapped) store
      let body = renderCommit c'
          new = hashObject ObjCommit body
          time = committerTime c'
          nowBackwards = newerThan newTimes time (cParents c')
          wasBackwards = newerThan originalTimes (committerTime c) (cParents c)
      unless (new == oid) $ void (writeObject store ObjCommit body)
      pure $
        Fold
          (Map.insert oid new mapping)
          (Map.insert new time newTimes)
          (if nowBackwards && not wasBackwards then new : backwards else backwards)

data Report = Report
  { rpCommits :: Int
  , rpChanged :: [(Oid, Oid)]
    -- ^ Oldest first.
  , rpOutOfOrder :: [Oid]
  , rpUpdates :: [RefUpdate]
  , rpSkippedTags :: [Ref]
  , rpBackup :: Maybe Int
    -- ^ Nothing for dry runs and rewrites that changed nothing.
  }

-- | Rewrite everything reachable from the given branches (all branches if none
-- given), then move those branches and any lightweight tags that pointed into
-- the rewritten history. A dry run writes the new commit objects (unreferenced,
-- so @git gc@ removes them) but leaves every ref alone.
rewriteBranches :: Store -> Bool -> Plan -> [ByteString] -> IO Report
rewriteBranches store dryRun plan requested = do
  unless dryRun (ensureClean store)
  branches <-
    if null requested
      then listRefs store ["refs/heads/"]
      else traverse (resolveBranch store) requested
  when (null branches) $ fail "no branches to rewrite"
  oids <- revList store (["--topo-order", "--reverse", "--end-of-options"] <> map (BC.unpack . refName) branches)
  history <- traverse (\o -> (o,) <$> readCommit store o) oids
  Rewritten mapping backwards <- rewriteCommits store (plan history) history
  tags <- listRefs store ["refs/tags/"]
  let (updates, skipped) = planRefUpdates mapping (branches <> tags)
      changed = [(o, n) | o <- oids, let n = mapping Map.! o, n /= o]
  backup <-
    if dryRun || null updates
      then pure Nothing
      else Just <$> applyRefUpdates store updates <* syncWorktree store
  pure (Report (length oids) changed backwards updates skipped backup)
