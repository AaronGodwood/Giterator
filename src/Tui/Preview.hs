-- | Dry-running a rewrite entirely in memory, for the TUI's live preview.
module Tui.Preview
  ( Outcome (..)
  , Preview (..)
  , PreviewCancelled (..)
  , previewRewrite
  ) where

import Control.Exception (Exception, finally, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Git.Rewrite
import Git.Store
import Git.Types

data Outcome
  = Unchanged
  | Became Oid Commit
  | Dropped

data Preview = Preview
  { pvOutcomes   :: Map Oid Outcome
    -- ^ Keyed by original commit id.
  , pvChanged    :: Int
  , pvDropped    :: Int
  , pvOutOfOrder :: Int
  , pvUnsigned   :: Int
  }

data PreviewCancelled = PreviewCancelled
  deriving (Show)

instance Exception PreviewCancelled

-- | Rewrite the history (oldest first) without writing anything or moving
-- any ref. @superseded@ is polled at the start of every commit, when the
-- cat-file pipe is idle, so abandoning a preview part-way can't leave the
-- pipe mid-response.
previewRewrite :: Store -> IO Bool -> Bool -> Plan -> [(Oid, Commit)] -> IO Preview
previewRewrite store superseded prune plan history =
  flip finally (discardObjects store) $ do
    transform <- plan history
    let guard = Transform $ \_ c -> c <$ liftIO (superseded >>= (`when` throwIO PreviewCancelled))
    Rewritten mapping backwards pruned unsigned <- rewriteCommits store prune (guard <> transform) history
    let droppedSet = Set.fromList pruned
        outcome oid
          | oid `Set.member` droppedSet = pure Dropped
          | Just new <- Map.lookup oid mapping, new /= oid = Became new <$> readCommit store new
          | otherwise = pure Unchanged
    outcomes <- traverse (\(oid, _) -> (oid,) <$> outcome oid) history
    pure
      Preview
        { pvOutcomes = Map.fromList outcomes
        , pvChanged = length [() | (_, Became _ _) <- outcomes]
        , pvDropped = length pruned
        , pvOutOfOrder = length backwards
        , pvUnsigned = length unsigned
        }
