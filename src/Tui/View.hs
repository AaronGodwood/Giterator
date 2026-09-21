module Tui.View
  ( draw
  , attributes
  ) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
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

draw :: Model -> [Widget Name]
draw m = [vBox [hBox [hLimitPercent 55 (history m), details m], footer]]

history :: Model -> Widget Name
history m = pane title body
  where
    count = length (mCommits m)
    title = case mStatus m of
      Ready -> branchLabel m <> " · " <> T.pack (show count) <> " commits"
      _ -> branchLabel m
    body = case mStatus m of
      Loading -> center (txt "loading history…")
      Failed err -> center (withAttr errorAttr (txtWrap (T.pack err)))
      Ready
        | count == 0 -> center (txt "no commits")
        | otherwise -> renderList row True (mCommits m)

-- | Hash, a merge mark, the author date, a mark when the committer date
-- differs (rebased or rewritten commits), and the subject.
row :: Bool -> (Oid, Commit) -> Widget Name
row isSelected (oid, c) =
  (if isSelected then forceAttr listSelectedFocusedAttr else id) $
    hBox
      [ withAttr hashAttr (txt (displayText (shortHex oid)))
      , withAttr markAttr (txt (if length (cParents c) > 1 then " M " else "   "))
      , withAttr dateAttr (txt (displayText (BC.take 16 (formatSignatureDate (cAuthor c)))))
      , withAttr markAttr (txt (if sigTime (cAuthor c) /= sigTime (cCommitter c) then " • " else "   "))
      , txt (displayText (BC.takeWhile (/= '\n') (cMessage c)))
      ]

details :: Model -> Widget Name
details m = case selected m of
  Nothing -> pane "details" (center (txt " "))
  Just commit@(oid, _) ->
    pane (displayText (shortHex oid) <> (if mRaw m then " · raw bytes" else "")) $
      viewport Details Vertical (vBox (map line (detailLines (mRaw m) commit)))
  where
    line t = if T.null t then txt " " else txtWrap t

footer :: Widget Name
footer =
  padLeft (Pad 1) . hBox $
    concatMap
      (\(k, what) -> [withAttr keyAttr (txt k), txt (" " <> what <> "   ")])
      [("↑↓ PgUp PgDn", "move"), ("r", "raw bytes"), ("J/K", "scroll details"), ("q", "quit")]

pane :: Text -> Widget Name -> Widget Name
pane title = withBorderStyle unicodeRounded . borderWithLabel (txt (" " <> title <> " "))

hashAttr, dateAttr, markAttr, keyAttr, errorAttr :: AttrName
hashAttr = attrName "hash"
dateAttr = attrName "date"
markAttr = attrName "mark"
keyAttr = attrName "key"
errorAttr = attrName "error"

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
    ]
