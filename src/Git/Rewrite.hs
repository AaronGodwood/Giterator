-- | The rewrite engine: a fold over commits, oldest first, that carries a map
-- from original to rewritten ids.
module Git.Rewrite
  ( GitM
  , Transform (..)
  , pureTransform
  , rewriteCommits
  , Report (..)
  , rewriteBranches
  ) where

import Control.Monad (foldM, unless, void, when, (>=>))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Git.Object (hashObject, renderCommit)
import Git.Refs
import Git.Store
import Git.Types

type GitM = ReaderT Store IO

-- | Composes left to right with '<>'; 'mempty' is the identity rewrite.
newtype Transform = Transform {runTransform :: Commit -> GitM Commit}

instance Semigroup Transform where
  Transform f <> Transform g = Transform (f >=> g)

instance Monoid Transform where
  mempty = Transform pure

pureTransform :: (Commit -> Commit) -> Transform
pureTransform f = Transform (pure . f)

-- | Commits must be in topological order, parents first. Parents are remapped
-- before the transform runs, so transforms see final parent ids (vanity mining
-- depends on this). Parents outside the list are kept as they are.
rewriteCommits :: Store -> Transform -> [Oid] -> IO (Map Oid Oid)
rewriteCommits store t = foldM step Map.empty
  where
    step mapping oid = do
      c <- readCommit store oid
      let remapped = c {cParents = map (\p -> Map.findWithDefault p p mapping) (cParents c)}
      c' <- runReaderT (runTransform t remapped) store
      let body = renderCommit c'
          new = hashObject ObjCommit body
      unless (new == oid) $ void (writeObject store ObjCommit body)
      pure $! Map.insert oid new mapping

data Report = Report
  { rpCommits :: Int
  , rpChanged :: [(Oid, Oid)]
    -- ^ Oldest first.
  , rpUpdates :: [RefUpdate]
  , rpSkippedTags :: [Ref]
  , rpBackup :: Maybe Int
    -- ^ Nothing for dry runs and rewrites that changed nothing.
  }

-- | Rewrite everything reachable from the given branches (all branches if none
-- given), then move those branches and any lightweight tags that pointed into
-- the rewritten history. A dry run writes the new commit objects (unreferenced,
-- so @git gc@ removes them) but leaves every ref alone.
rewriteBranches :: Store -> Bool -> Transform -> [ByteString] -> IO Report
rewriteBranches store dryRun t requested = do
  unless dryRun (ensureClean store)
  branches <-
    if null requested
      then listRefs store ["refs/heads/"]
      else traverse (resolveBranch store) requested
  when (null branches) $ fail "no branches to rewrite"
  oids <- revList store (["--topo-order", "--reverse", "--end-of-options"] <> map (BC.unpack . refName) branches)
  mapping <- rewriteCommits store t oids
  tags <- listRefs store ["refs/tags/"]
  let (updates, skipped) = planRefUpdates mapping (branches <> tags)
      changed = [(o, n) | o <- oids, let n = mapping Map.! o, n /= o]
  backup <-
    if dryRun || null updates
      then pure Nothing
      else Just <$> applyRefUpdates store updates <* syncWorktree store
  pure (Report (length oids) changed updates skipped backup)
