-- | Wiring: brick on the main thread, and one worker thread that owns the
-- 'Store' (its cat-file pipe can't be shared) and does all git work, so the
-- screen stays responsive while large histories load.
module Tui.Run
  ( runTui
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.List (handleListEvent, handleListEventVi)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Git.Refs (Ref (..), currentBranch, resolveBranch)
import Git.Store
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Tui.Model
import Tui.View

newtype Request = LoadHistory ByteString

-- | Browse a branch (default: the checked-out one).
runTui :: FilePath -> Maybe ByteString -> IO ()
runTui repo requested = withStore repo $ \store -> do
  branch <- case requested of
    Just b -> refName <$> resolveBranch store b
    Nothing -> maybe (fail "HEAD is detached; name a branch") pure =<< currentBranch store
  events <- newBChan 16
  requests <- newChan
  writeChan requests (LoadHistory branch)
  bracket (forkIO (worker store requests events)) killThread $ \_ -> do
    let buildVty = mkVty V.defaultConfig
    vty <- buildVty
    void (customMain vty buildVty (Just events) app (initialModel branch))

worker :: Store -> Chan Request -> BChan WorkerEvent -> IO ()
worker store requests events = forever $
  readChan requests >>= \case
    LoadHistory branch -> do
      result <- try $ do
        oids <- revList store ["--topo-order", "--end-of-options", BC.unpack branch]
        traverse (\o -> (o,) <$> readCommit store o) oids
      writeBChan events $ case result of
        Right commits -> HistoryLoaded commits
        Left (e :: SomeException) -> WorkerFailed (displayException e)

app :: App Model WorkerEvent Name
app =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const attributes
    }

handleEvent :: BrickEvent Name WorkerEvent -> EventM Name Model ()
handleEvent = \case
  AppEvent e -> modify (applyWorker e)
  VtyEvent ev -> case keyCommand ev of
    Just Quit -> halt
    Just ToggleRaw -> modify toggleRaw
    Just (ScrollDetails n) -> vScrollBy (viewportScroll Details) n
    Nothing -> do
      before <- gets (fmap fst . selected)
      m <- get
      commits <- nestEventM' (mCommits m) (handleListEventVi handleListEvent ev)
      put m {mCommits = commits}
      after <- gets (fmap fst . selected)
      when (before /= after) $ vScrollToBeginning (viewportScroll Details)
  _ -> pure ()
