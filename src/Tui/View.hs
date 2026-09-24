module Tui.View
  ( draw
  , attributes
  ) where

import Brick
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr, renderForm)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.List (listSelectedFocusedAttr, renderList)
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text qualified as T
import Git.Date (formatSignatureDate)
import Git.Types
import Graphics.Vty qualified as V
import Tui.Model
import Tui.Preview

-- | Brick draws the first layer on top.
draw :: Model -> [Widget Name]
draw m = overlay m <> [vBox [hBox [hLimitPercent 55 (history m), rightPane], statusLine m, footer m]]
  where
    rightPane = case mPanel m of
      DetailsPanel -> details m
      FormPanel -> transformForm m

overlay :: Model -> [Widget Name]
overlay m = case mOverlay m of
  NoOverlay -> []
  HelpOverlay -> [box "keys · any key to close" help]
  ConfirmOverlay action -> [box "confirm" (confirmText action)]
  where
    box title body = centerLayer (hLimit 72 (pane title (padAll 1 body)))
    confirmText action =
      vBox
        [ txtWrap $ case action of
            ApplyRewrite _ ->
              "Rewrite " <> branchLabel m <> summaryText <> "? The old history is kept as a backup, so u can undo it."
            UndoRewrite -> "Undo the most recent rewrite, restoring the refs it moved?"
        , txt " "
        , hBox [withAttr keyAttr (txt "y / Enter"), txt " go ahead    ", withAttr keyAttr (txt "n / Esc"), txt " cancel"]
        ]
    summaryText = case mPreview m of
      PreviewShown p -> " (" <> previewCounts p <> ")"
      _ -> ""
    help =
      vBox
        [ hBox [withAttr keyAttr (txt (T.justifyLeft 16 ' ' k)), txt what]
        | (k, what) <-
            [ ("↑↓ PgUp PgDn", "move through history (also j k g G)")
            , ("/", "filter by hash, subject or author (Enter keeps, Esc clears)")
            , ("d", "show author or committer dates")
            , ("r", "readable view or raw object bytes")
            , ("J / K", "scroll the details pane")
            , ("t", "transform form with live preview (Esc returns)")
            , ("a", "apply the previewed rewrite")
            , ("u", "undo the most recent rewrite")
            , ("?", "this help")
            , ("q / Esc", "quit")
            ]
        ]

history :: Model -> Widget Name
history m = pane title body
  where
    shown = length (mCommits m)
    title = case mStatus m of
      Ready ->
        branchLabel m
          <> " · " <> T.pack (show (length (mAll m))) <> " commits"
          <> (if T.null (mFilter m) then "" else " · " <> T.pack (show shown) <> " match /" <> mFilter m)
          <> (if mDates m == CommitterDates then " · committer dates" else "")
          <> previewSummary (mPreview m)
      _ -> branchLabel m
    body = case mStatus m of
      Loading -> center (txt "loading history…")
      Failed err -> center (withAttr errorAttr (txtWrap (T.pack err)))
      Ready
        | null (mAll m) -> center (txt "no commits")
        | shown == 0 -> center (txt "no commits match the filter")
        | otherwise -> renderList (row m) True (mCommits m)

previewSummary :: PreviewState -> Text
previewSummary = \case
  NoPreview -> ""
  PreviewRunning -> " · previewing…"
  PreviewFailed _ -> " · preview failed"
  PreviewShown p -> " · preview: " <> previewCounts p

previewCounts :: Preview -> Text
previewCounts p =
  T.intercalate ", " . filter (not . T.null) $
    [ count (pvChanged p) "changed"
    , count (pvDropped p) "dropped"
    , count (pvOutOfOrder p) "out of order"
    , count (pvUnsigned p) "unsigned"
    ]
  where
    count n what
      | n == 0 && what /= "changed" = ""
      | otherwise = T.pack (show n) <> " " <> what

-- | Hash, a merge mark, the date, a mark when author and committer dates
-- differ (rebased or rewritten commits), and the subject. With a preview,
-- the hash and date columns show the rewritten commit instead.
row :: Model -> Bool -> (Oid, Commit) -> Widget Name
row m isSelected (oid, c) =
  (if isSelected then forceAttr listSelectedFocusedAttr else id) . hBox $ case selectedOutcome m oid of
    Just (Became new c') ->
      [ withAttr newAttr (hash new)
      , mergeMark
      , withAttr (if dateOf c' == dateOf c then dateStyle else newAttr) (date c')
      , rebaseMark c'
      , subject
      ]
    Just Dropped -> [withAttr droppedAttr (txt "dropped"), mergeMark, withAttr droppedAttr (date c), txt "   ", subject]
    _ -> [withAttr hashAttr (hash oid), mergeMark, withAttr dateStyle (date c), rebaseMark c, subject]
  where
    dateOf = if mDates m == AuthorDates then cAuthor else cCommitter
    dateStyle = if mDates m == AuthorDates then dateAttr else committerAttr
    hash = txt . displayText . shortHex
    date = txt . displayText . BC.take 16 . formatSignatureDate . dateOf
    mergeMark = withAttr markAttr (txt (if length (cParents c) > 1 then " M " else "   "))
    rebaseMark x = withAttr markAttr (txt (if sigTime (cAuthor x) /= sigTime (cCommitter x) then " • " else "   "))
    subject = txt (displayText (BC.takeWhile (/= '\n') (cMessage c)))

details :: Model -> Widget Name
details m = case selected m of
  Nothing -> pane "details" (center (txt " "))
  Just commit@(oid, c) ->
    pane (displayText (shortHex oid) <> (if mRaw m then " · raw bytes" else "")) . viewport Details Vertical . vBox $
      case selectedOutcome m oid of
        Just outcome -> map (withAttr newAttr . line) (previewLines c outcome) <> [hBorder] <> body commit
        Nothing -> body commit
  where
    body commit = map (\t -> (if "committer" `T.isPrefixOf` t then withAttr committerAttr else id) (line t)) (detailLines (mRaw m) commit)
    line t = if T.null t then txt " " else txtWrap t

transformForm :: Model -> Widget Name
transformForm m =
  pane "transform · live preview" . vBox $
    [renderForm (mForm m), hBorder]
      <> case mErrors m of
        [] -> [withAttr dateAttr (txtWrap "Blank fields are ignored. Examples: shift 3d, spread 2024-01-01..2024-03-01, work hours 09:00-17:30, timezone +0900, replace old=>new.")]
        errors -> [withAttr errorAttr (txtWrap (T.pack e)) | (_, e) <- errors]
      <> case mPreview m of
        PreviewFailed err -> [withAttr errorAttr (txtWrap ("preview failed: " <> T.pack err))]
        _ -> []

-- | The search box while typing, otherwise the latest message.
statusLine :: Model -> Widget Name
statusLine m
  | mSearching m = padLeft (Pad 1) (withAttr keyAttr (txt "/") <+> showCursor CommitList (Location (T.length (mFilter m), 0)) (txt (mFilter m <> " ")))
  | mBusy m = padLeft (Pad 1) (withAttr keyAttr (txt "working…"))
  | otherwise = padLeft (Pad 1) $ case mMessage m of
      Just (Info t) -> withAttr newAttr (txt t)
      Just (Problem t) -> withAttr errorAttr (txt t)
      Nothing -> txt " "

footer :: Model -> Widget Name
footer m =
  padLeft (Pad 1) . hBox . concatMap hint $ case (mSearching m, mPanel m) of
    (True, _) -> [("Enter", "keep filter"), ("Esc", "clear filter")]
    (_, DetailsPanel) -> [("↑↓", "move"), ("/", "filter"), ("t", "transform"), ("a", "apply"), ("u", "undo"), ("?", "help"), ("q", "quit")]
    (_, FormPanel) -> [("Tab/Shift-Tab", "next/previous field"), ("Space", "toggle"), ("Esc", "back to history")]
  where
    hint (k, what) = [withAttr keyAttr (txt k), txt (" " <> what <> "   ")]

pane :: Text -> Widget Name -> Widget Name
pane title = withBorderStyle unicodeRounded . borderWithLabel (txt (" " <> title <> " "))

hashAttr, dateAttr, committerAttr, markAttr, keyAttr, errorAttr, newAttr, droppedAttr :: AttrName
hashAttr = attrName "hash"
dateAttr = attrName "date"
committerAttr = attrName "committerDate"
markAttr = attrName "mark"
keyAttr = attrName "key"
errorAttr = attrName "error"
newAttr = attrName "new"
droppedAttr = attrName "dropped"

attributes :: AttrMap
attributes =
  attrMap
    V.defAttr
    [ (listSelectedFocusedAttr, V.black `on` V.cyan)
    , (hashAttr, fg V.yellow)
    , (dateAttr, fg V.brightBlack)
    , (committerAttr, fg V.blue)
    , (markAttr, fg V.magenta)
    , (keyAttr, fg V.cyan `V.withStyle` V.bold)
    , (errorAttr, fg V.red)
    , (newAttr, fg V.green)
    , (droppedAttr, fg V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    ]
