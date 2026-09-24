-- | The TUI's state and everything that changes it without IO, so it can be
-- tested without a terminal.
module Tui.Model
  ( Name (..)
  , Status (..)
  , Panel (..)
  , PreviewState (..)
  , Overlay (..)
  , Action (..)
  , Message (..)
  , DateColumn (..)
  , Model (..)
  , WorkerEvent (..)
  , Command (..)
  , initialModel
  , applyWorker
  , formEdited
  , toggleRaw
  , toggleDates
  , selected
  , selectedOutcome
  , keyCommand
  , searchKey
  , setFilter
  , matches
  , requestApply
  , requestUndo
  , confirmKey
  , applySummary
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
import Data.Either (fromLeft, fromRight)
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector qualified as Vec
import Git.Date (formatSignatureDate)
import Git.Object (isSigned, renderCommit)
import Git.Pretty (prettyCommit, showRaw)
import Git.Refs (Ref (..))
import Git.Rewrite (Report (..))
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

-- | Something drawn over everything else that takes the keyboard until closed.
data Overlay = NoOverlay | HelpOverlay | ConfirmOverlay Action

data Action = ApplyRewrite PreviewSpec | UndoRewrite

data Message = Info Text | Problem Text
  deriving (Eq, Show)

data DateColumn = AuthorDates | CommitterDates
  deriving (Eq, Show)

data Model = Model
  { mBranch    :: ByteString
    -- ^ Full ref name, e.g. @refs/heads/main@.
  , mStatus    :: Status
  , mAll       :: [(Oid, Commit)]
    -- ^ The whole history, newest first; 'mCommits' shows the filtered part.
  , mCommits   :: List Name (Oid, Commit)
  , mFilter    :: Text
  , mSearching :: Bool
  , mDates     :: DateColumn
  , mRaw       :: Bool
    -- ^ Show the selected commit's exact bytes instead of the readable view.
  , mPanel     :: Panel
  , mOverlay   :: Overlay
  , mForm      :: Form FormInput WorkerEvent Name
  , mErrors    :: [(Name, String)]
  , mPreview   :: PreviewState
  , mGen       :: Int
    -- ^ Bumped for every new preview request; results for older ones are ignored.
  , mPreviewed :: Maybe FormInput
    -- ^ The input behind the current preview, so moving between fields doesn't re-run it.
  , mBusy      :: Bool
    -- ^ An apply or undo is running.
  , mMessage   :: Maybe Message
  }

-- | Results sent back by the worker thread that owns the git process.
data WorkerEvent
  = HistoryLoaded [(Oid, Commit)]
  | WorkerFailed String
  | PreviewDone Int (Either String Preview)
  | Applied (Either String Text)
  | Undone (Either String Text)

-- | Keys the history list handles itself; anything else goes to the list
-- widget (arrows, PgUp/PgDn, vi keys).
data Command
  = Quit
  | ToggleRaw
  | ToggleDates
  | ScrollDetails Int
  | OpenForm
  | StartSearch
  | ShowHelp
  | Apply
  | Undo
  deriving (Eq, Show)

initialModel :: ByteString -> Model
initialModel branch =
  Model
    { mBranch = branch
    , mStatus = Loading
    , mAll = []
    , mCommits = list CommitList mempty 1
    , mFilter = ""
    , mSearching = False
    , mDates = AuthorDates
    , mRaw = False
    , mPanel = DetailsPanel
    , mOverlay = NoOverlay
    , mForm = mkForm emptyInput
    , mErrors = []
    , mPreview = NoPreview
    , mGen = 0
    , mPreviewed = Nothing
    , mBusy = False
    , mMessage = Nothing
    }

applyWorker :: WorkerEvent -> Model -> Model
applyWorker event m = case event of
  HistoryLoaded commits -> refilter Nothing m {mStatus = Ready, mAll = commits}
  WorkerFailed err -> m {mStatus = Failed err}
  PreviewDone gen result
    | gen /= mGen m -> m
    | otherwise -> m {mPreview = either PreviewFailed PreviewShown result}
  -- The preview described the old history, so it goes either way; after a
  -- successful apply the form goes too, or the same change would be offered again.
  Applied (Right summary) ->
    finished (Info summary) m {mForm = mkForm emptyInput, mErrors = [], mPanel = DetailsPanel}
  Applied (Left err) -> m {mBusy = False, mMessage = Just (Problem (T.pack err))}
  Undone (Right summary) -> finished (Info summary) m
  Undone (Left err) -> m {mBusy = False, mMessage = Just (Problem (T.pack err))}
  where
    finished msg model = model {mBusy = False, mMessage = Just msg, mPreview = NoPreview, mPreviewed = Nothing}

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

toggleDates :: Model -> Model
toggleDates m = m {mDates = if mDates m == AuthorDates then CommitterDates else AuthorDates}

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
  V.EvKey (V.KChar 'd') [] -> Just ToggleDates
  V.EvKey (V.KChar 't') [] -> Just OpenForm
  V.EvKey (V.KChar '/') [] -> Just StartSearch
  V.EvKey (V.KChar 'a') [] -> Just Apply
  V.EvKey (V.KChar 'u') [] -> Just Undo
  -- Shifted characters arrive with or without MShift depending on the terminal.
  V.EvKey (V.KChar '?') _ -> Just ShowHelp
  V.EvKey (V.KChar 'J') _ -> Just (ScrollDetails 1)
  V.EvKey (V.KChar 'K') _ -> Just (ScrollDetails (-1))
  _ -> Nothing

-- Search ---------------------------------------------------------------------

-- | Typing edits the filter live; Enter keeps it, Esc clears it.
searchKey :: V.Event -> Model -> Model
searchKey ev m = case ev of
  V.EvKey V.KEnter [] -> m {mSearching = False}
  V.EvKey V.KEsc [] -> setFilter "" m {mSearching = False}
  V.EvKey V.KBS [] -> setFilter (T.dropEnd 1 (mFilter m)) m
  V.EvKey (V.KChar ch) mods | V.MCtrl `notElem` mods, V.MMeta `notElem` mods -> setFilter (T.snoc (mFilter m) ch) m
  _ -> m

setFilter :: Text -> Model -> Model
setFilter q m = refilter (fst <$> selected m) m {mFilter = q}

-- | Rebuild the visible list, keeping the selection on the same commit when
-- it's still shown.
refilter :: Maybe Oid -> Model -> Model
refilter keep m = m {mCommits = listReplace (Vec.fromList shown) index (mCommits m)}
  where
    shown = filter (matches (mFilter m)) (mAll m)
    index = case keep >>= \o -> findIndex ((== o) . fst) shown of
      Just i -> Just i
      Nothing -> if null shown then Nothing else Just 0

-- | Case-insensitive match on the hash, subject or author.
matches :: Text -> (Oid, Commit) -> Bool
matches q (oid, c)
  | T.null q = True
  | otherwise = any (T.isInfixOf (T.toLower q) . T.toLower) [displayText (oidToHex oid), subject, author]
  where
    subject = displayText (BC.takeWhile (/= '\n') (cMessage c))
    author = displayText (sigName (cAuthor c))

-- Apply and undo -------------------------------------------------------------

requestApply :: Model -> Model
requestApply m = case (mPreview m, mPreviewed m >>= fromRight Nothing . buildSpec) of
  (PreviewShown p, Just spec)
    | pvChanged p + pvDropped p > 0 -> m {mOverlay = ConfirmOverlay (ApplyRewrite spec)}
    | otherwise -> m {mMessage = Just (Info "the preview doesn't change anything")}
  (PreviewRunning, _) -> m {mMessage = Just (Info "wait for the preview to finish first")}
  _ -> m {mMessage = Just (Problem "nothing to apply yet: press t and set up a transform")}

requestUndo :: Model -> Model
requestUndo m = m {mOverlay = ConfirmOverlay UndoRewrite}

-- | In a confirmation: Just True to go ahead, Just False to back out.
confirmKey :: V.Event -> Maybe Bool
confirmKey = \case
  V.EvKey (V.KChar 'y') [] -> Just True
  V.EvKey V.KEnter [] -> Just True
  V.EvKey (V.KChar 'n') [] -> Just False
  V.EvKey V.KEsc [] -> Just False
  _ -> Nothing

applySummary :: Report -> Text
applySummary r =
  T.intercalate " · " $
    [ "rewrote " <> count (length (rpChanged r)) "commit"
        <> (if null (rpPruned r) then "" else ", dropped " <> count (length (rpPruned r)) "empty commit")
    ]
      <> case rpBackup r of
        Just n -> ["backup #" <> T.pack (show n) <> " (u to undo)"]
        Nothing -> ["nothing changed"]
      <> [count (length (rpOutOfOrder r)) "commit" <> " now dated before a parent" | not (null (rpOutOfOrder r))]
      <> [count (length (rpUnsigned r)) "signature" <> " removed" | not (null (rpUnsigned r))]
      <> ["annotated tag " <> displayText (refName t) <> " not moved" | t <- rpSkippedTags r]
  where
    count n what = T.pack (show n) <> " " <> what <> (if n == 1 then "" else "s")

-- Display --------------------------------------------------------------------

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
