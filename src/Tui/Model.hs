-- | The TUI's state and everything that changes it without IO, so it can be
-- tested without a terminal.
module Tui.Model
  ( Name (..)
  , Status (..)
  , Panel (..)
  , PreviewState (..)
  , Model (..)
  , WorkerEvent (..)
  , Command (..)
  , initialModel
  , applyWorker
  , formEdited
  , toggleRaw
  , selected
  , selectedOutcome
  , keyCommand
  , branchLabel
  , detailLines
  , previewLines
  , displayText
  ) where

import Brick.Forms (Form, formState, setFieldValid)
import Brick.Widgets.List (List, list, listReplace, listSelectedElement)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isControl)
import Data.Either (fromLeft)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector qualified as Vec
import Git.Date (formatSignatureDate)
import Git.Object (isSigned, renderCommit)
import Git.Pretty (prettyCommit, showRaw)
import Git.Types
import Graphics.Vty qualified as V
import Tui.Form
import Tui.Name
import Tui.Preview

data Status = Loading | Ready | Failed String
  deriving (Eq, Show)

-- | What the right-hand pane shows. The form panel also takes keyboard focus.
data Panel = DetailsPanel | FormPanel
  deriving (Eq, Show)

data PreviewState
  = NoPreview
  | PreviewRunning
  | PreviewShown Preview
  | PreviewFailed String

data Model = Model
  { mBranch  :: ByteString
    -- ^ Full ref name, e.g. @refs/heads/main@.
  , mStatus  :: Status
  , mCommits :: List Name (Oid, Commit)
    -- ^ Newest first, as @git log@ shows them.
  , mRaw     :: Bool
    -- ^ Show the selected commit's exact bytes instead of the readable view.
  , mPanel   :: Panel
  , mForm    :: Form FormInput WorkerEvent Name
  , mErrors  :: [(Name, String)]
  , mPreview :: PreviewState
  , mGen     :: Int
    -- ^ Bumped for every new preview request; results for older ones are ignored.
  , mPreviewed :: Maybe FormInput
    -- ^ The input behind the current preview, so moving between fields doesn't re-run it.
  }

-- | Results sent back by the worker thread that owns the git process.
data WorkerEvent
  = HistoryLoaded [(Oid, Commit)]
  | WorkerFailed String
  | PreviewDone Int (Either String Preview)

-- | Keys the list handles itself; anything else goes to the list widget
-- (arrows, PgUp/PgDn, vi keys).
data Command = Quit | ToggleRaw | ScrollDetails Int | OpenForm
  deriving (Eq, Show)

initialModel :: ByteString -> Model
initialModel branch =
  Model
    { mBranch = branch
    , mStatus = Loading
    , mCommits = list CommitList mempty 1
    , mRaw = False
    , mPanel = DetailsPanel
    , mForm = mkForm emptyInput
    , mErrors = []
    , mPreview = NoPreview
    , mGen = 0
    , mPreviewed = Nothing
    }

applyWorker :: WorkerEvent -> Model -> Model
applyWorker event m = case event of
  HistoryLoaded commits ->
    m
      { mStatus = Ready
      , mCommits = listReplace (Vec.fromList commits) (if null commits then Nothing else Just 0) (mCommits m)
      }
  WorkerFailed err -> m {mStatus = Failed err}
  PreviewDone gen result
    | gen /= mGen m -> m
    | otherwise -> m {mPreview = either PreviewFailed PreviewShown result}

-- | After the form changes: mark invalid fields, and decide whether a new
-- preview is needed. Returns the preview to request, tagged with its generation.
formEdited :: Model -> (Model, Maybe (Int, PreviewSpec))
formEdited m
  | Just input == mPreviewed m = (validated, Nothing)
  | otherwise = case buildSpec input of
      -- Keep showing the last good preview while a field is half-typed.
      Left _ -> (validated, Nothing)
      Right Nothing -> (next {mPreview = NoPreview}, Nothing)
      Right (Just spec) -> (next {mPreview = PreviewRunning}, Just (gen, spec))
  where
    input = formState (mForm m)
    errors = fromLeft [] (buildSpec input)
    validated =
      m
        { mErrors = errors
        , mForm = foldr (\name -> setFieldValid (name `notElem` map fst errors) name) (mForm m) textFields
        }
    gen = mGen m + 1
    next = validated {mGen = gen, mPreviewed = Just input}
    textFields = [FSpread, FShift, FJitter, FSeed, FWorkHours, FTz, FReplace, FReplaceRegex, FPath, FDelete, FRename]

toggleRaw :: Model -> Model
toggleRaw m = m {mRaw = not (mRaw m)}

selected :: Model -> Maybe (Oid, Commit)
selected = fmap snd . listSelectedElement . mCommits

selectedOutcome :: Model -> Oid -> Maybe Outcome
selectedOutcome m oid = case mPreview m of
  PreviewShown p -> Map.lookup oid (pvOutcomes p)
  _ -> Nothing

keyCommand :: V.Event -> Maybe Command
keyCommand = \case
  V.EvKey (V.KChar 'q') [] -> Just Quit
  V.EvKey V.KEsc [] -> Just Quit
  V.EvKey (V.KChar 'r') [] -> Just ToggleRaw
  V.EvKey (V.KChar 't') [] -> Just OpenForm
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

-- | What the previewed rewrite would do to one commit, field by field.
previewLines :: Commit -> Outcome -> [Text]
previewLines old = \case
  Unchanged -> ["unchanged by this rewrite"]
  Dropped -> ["dropped: left with no changes"]
  Became new c ->
    ["becomes   " <> displayText (oidToHex new)]
      <> changed "author" (date (cAuthor old)) (date (cAuthor c))
      <> changed "committer" (date (cCommitter old)) (date (cCommitter c))
      <> ["files     changed" | cTree old /= cTree c]
      <> ["message   changed" | cMessage old /= cMessage c]
      <> ["signature removed" | isSigned old && not (isSigned c)]
  where
    date = displayText . formatSignatureDate
    changed label before after
      | before == after = []
      | otherwise = [T.justifyLeft 10 ' ' label <> before <> " → " <> after]

-- | Git text is bytes, usually UTF-8. Decode leniently, expand tabs, and make
-- any other control characters visible: terminals would act on them.
displayText :: ByteString -> Text
displayText = T.concatMap clean . decodeUtf8With lenientDecode
  where
    clean '\t' = "    "
    clean ch
      | isControl ch = "\xFFFD"
      | otherwise = T.singleton ch
