module Tui.View
  ( draw
  , attributes
  ) where

import Brick
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr, renderForm)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import Brick.Widgets.List (listSelectedFocusedAttr, renderList)
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text qualified as T
import Git.Date (formatSignatureDate)
import Git.Types
import Graphics.Vty qualified as V
import Tui.Model
import Tui.Preview

draw :: Model -> [Widget Name]
draw m = [vBox [hBox [hLimitPercent 55 (history m), rightPane], footer m]]
  where
    rightPane = case mPanel m of
      DetailsPanel -> details m
      FormPanel -> transformForm m

history :: Model -> Widget Name
history m = pane title body
  where
    count = length (mCommits m)
    title = case mStatus m of
      Ready -> branchLabel m <> " · " <> T.pack (show count) <> " commits" <> previewSummary (mPreview m)
      _ -> branchLabel m
    body = case mStatus m of
      Loading -> center (txt "loading history…")
      Failed err -> center (withAttr errorAttr (txtWrap (T.pack err)))
      Ready
        | count == 0 -> center (txt "no commits")
        | otherwise -> renderList (row m) True (mCommits m)

previewSummary :: PreviewState -> Text
previewSummary = \case
  NoPreview -> ""
  PreviewRunning -> " · previewing…"
  PreviewFailed _ -> " · preview failed"
  PreviewShown p ->
    " · preview: " <> T.intercalate ", " (filter (not . T.null)
      [ count (pvChanged p) "changed"
      , count (pvDropped p) "dropped"
      , count (pvOutOfOrder p) "out of order"
      , count (pvUnsigned p) "unsigned"
      ])
  where
    count n what
      | n == 0 && what /= "changed" = ""
      | otherwise = T.pack (show n) <> " " <> what

-- | Hash, a merge mark, the author date, a mark when the committer date
-- differs (rebased or rewritten commits), and the subject. With a preview,
-- the hash and date columns show the rewritten commit instead.
row :: Model -> Bool -> (Oid, Commit) -> Widget Name
row m isSelected (oid, c) =
  (if isSelected then forceAttr listSelectedFocusedAttr else id) . hBox $ case selectedOutcome m oid of
    Just (Became new c') ->
      [ withAttr newAttr (hash new)
      , mergeMark
      , withAttr (if cAuthor c' == cAuthor c then dateAttr else newAttr) (date c')
      , rebaseMark c'
      , subject
      ]
    Just Dropped -> [withAttr droppedAttr (txt "dropped"), mergeMark, withAttr droppedAttr (date c), txt "   ", subject]
    _ -> [withAttr hashAttr (hash oid), mergeMark, withAttr dateAttr (date c), rebaseMark c, subject]
  where
    hash = txt . displayText . shortHex
    date = txt . displayText . BC.take 16 . formatSignatureDate . cAuthor
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
    body commit = map line (detailLines (mRaw m) commit)
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

footer :: Model -> Widget Name
footer m =
  padLeft (Pad 1) . hBox . concatMap hint $ case mPanel m of
    DetailsPanel -> [("↑↓ PgUp PgDn", "move"), ("t", "transform"), ("r", "raw bytes"), ("J/K", "scroll details"), ("q", "quit")]
    FormPanel -> [("Tab/Shift-Tab", "next/previous field"), ("Space", "toggle"), ("Esc", "back to history")]
  where
    hint (k, what) = [withAttr keyAttr (txt k), txt (" " <> what <> "   ")]

pane :: Text -> Widget Name -> Widget Name
pane title = withBorderStyle unicodeRounded . borderWithLabel (txt (" " <> title <> " "))

hashAttr, dateAttr, markAttr, keyAttr, errorAttr, newAttr, droppedAttr :: AttrName
hashAttr = attrName "hash"
dateAttr = attrName "date"
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
    , (markAttr, fg V.magenta)
    , (keyAttr, fg V.cyan `V.withStyle` V.bold)
    , (errorAttr, fg V.red)
    , (newAttr, fg V.green)
    , (droppedAttr, fg V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    ]
