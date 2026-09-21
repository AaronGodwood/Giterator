-- | The TUI's state and everything that changes it without IO, so it can be
-- tested without a terminal.
module Tui.Model
  ( Name (..)
  , Status (..)
  , Model (..)
  , WorkerEvent (..)
  , Command (..)
  , initialModel
  , applyWorker
  , toggleRaw
  , selected
  , keyCommand
  , branchLabel
  , detailLines
  , displayText
  ) where

import Brick.Widgets.List (List, list, listReplace, listSelectedElement)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isControl)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector qualified as Vec
import Git.Object (renderCommit)
import Git.Pretty (prettyCommit, showRaw)
import Git.Types
import Graphics.Vty qualified as V

data Name = CommitList | Details
  deriving (Eq, Ord, Show)

data Status = Loading | Ready | Failed String
  deriving (Eq, Show)

data Model = Model
  { mBranch  :: ByteString
    -- ^ Full ref name, e.g. @refs/heads/main@.
  , mStatus  :: Status
  , mCommits :: List Name (Oid, Commit)
    -- ^ Newest first, as @git log@ shows them.
  , mRaw     :: Bool
    -- ^ Show the selected commit's exact bytes instead of the readable view.
  }

-- | Results sent back by the worker thread that owns the git process.
data WorkerEvent
  = HistoryLoaded [(Oid, Commit)]
  | WorkerFailed String

-- | Keys the TUI handles itself; anything else goes to the list (arrows, PgUp/PgDn, vi keys).
data Command = Quit | ToggleRaw | ScrollDetails Int
  deriving (Eq, Show)

initialModel :: ByteString -> Model
initialModel branch = Model branch Loading (list CommitList mempty 1) False

applyWorker :: WorkerEvent -> Model -> Model
applyWorker event m = case event of
  HistoryLoaded commits ->
    m
      { mStatus = Ready
      , mCommits = listReplace (Vec.fromList commits) (if null commits then Nothing else Just 0) (mCommits m)
      }
  WorkerFailed err -> m {mStatus = Failed err}

toggleRaw :: Model -> Model
toggleRaw m = m {mRaw = not (mRaw m)}

selected :: Model -> Maybe (Oid, Commit)
selected = fmap snd . listSelectedElement . mCommits

keyCommand :: V.Event -> Maybe Command
keyCommand = \case
  V.EvKey (V.KChar 'q') [] -> Just Quit
  V.EvKey V.KEsc [] -> Just Quit
  V.EvKey (V.KChar 'r') [] -> Just ToggleRaw
  -- Shifted letters arrive with or without MShift depending on the terminal.
  V.EvKey (V.KChar 'J') _ -> Just (ScrollDetails 1)
  V.EvKey (V.KChar 'K') _ -> Just (ScrollDetails (-1))
  _ -> Nothing

branchLabel :: Model -> Text
branchLabel m = displayText (fromMaybe b (BS.stripPrefix "refs/heads/" b))
  where
    b = mBranch m

detailLines :: Bool -> (Oid, Commit) -> [Text]
detailLines raw (oid, c) =
  map displayText . BC.lines $
    if raw then showRaw oid ObjCommit (renderCommit c) else prettyCommit oid c

-- | Git text is bytes, usually UTF-8. Decode leniently, expand tabs, and make
-- any other control characters visible: terminals would act on them.
displayText :: ByteString -> Text
displayText = T.concatMap clean . decodeUtf8With lenientDecode
  where
    clean '\t' = "    "
    clean ch
      | isControl ch = "\xFFFD"
      | otherwise = T.singleton ch
