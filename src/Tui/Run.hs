-- | Wiring: brick on the main thread, and one worker thread that owns the
-- 'Store' (its cat-file pipe can't be shared) and does all git work, so the
-- screen stays responsive while histories load and previews run.
module Tui.Run
  ( runTui
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus (focusRingCursor)
import Brick.Forms (formFocus, handleFormEvent)
import Brick.Widgets.List (handleListEvent, handleListEventVi)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, bracket, displayException, fromException, try)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Git.Refs (Ref (..), currentBranch, resolveBranch)
import Git.Store
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Tui.Form (PreviewSpec (..), specPlan)
import Tui.Model
import Tui.Preview
import Tui.View

data Request
  = LoadHistory ByteString
  | RunPreview Int PreviewSpec

-- | Browse a branch (default: the checked-out one).
runTui :: FilePath -> Maybe ByteString -> IO ()
runTui repo requested = withStore repo $ \store -> do
  branch <- case requested of
    Just b -> refName <$> resolveBranch store b
    Nothing -> maybe (fail "HEAD is detached; name a branch") pure =<< currentBranch store
  events <- newBChan 16
  requests <- newChan
  -- The newest preview generation; the worker abandons any older preview.
  latest <- newIORef 0
  writeChan requests (LoadHistory branch)
  bracket (forkIO (worker store latest requests events)) killThread $ \_ -> do
    let buildVty = mkVty V.defaultConfig
    vty <- buildVty
    void (customMain vty buildVty (Just events) (app latest requests) (initialModel branch))

worker :: Store -> IORef Int -> Chan Request -> BChan WorkerEvent -> IO ()
worker store latest requests events = loop []
  where
    loop history =
      readChan requests >>= \case
        LoadHistory branch -> do
          result <- try $ do
            oids <- revList store ["--topo-order", "--end-of-options", BC.unpack branch]
            traverse (\o -> (o,) <$> readCommit store o) oids
          case result of
            Right commits -> do
              writeBChan events (HistoryLoaded commits)
              loop (reverse commits)
            Left (e :: SomeException) -> do
              writeBChan events (WorkerFailed (displayException e))
              loop history
        RunPreview gen spec -> do
          let superseded = (/= gen) <$> readIORef latest
          result <- try (previewRewrite store superseded (psPrune spec) (specPlan spec) history)
          case result of
            Right preview -> writeBChan events (PreviewDone gen (Right preview))
            Left e
              | Just PreviewCancelled <- fromException e -> pure ()
              | otherwise -> writeBChan events (PreviewDone gen (Left (displayException e)))
          loop history

app :: IORef Int -> Chan Request -> App Model WorkerEvent Name
app latest requests =
  App
    { appDraw = draw
    , appChooseCursor = \m -> case mPanel m of
        FormPanel -> focusRingCursor formFocus (mForm m)
        DetailsPanel -> neverShowCursor m
    , appHandleEvent = handleEvent latest requests
    , appStartEvent = pure ()
    , appAttrMap = const attributes
    }

handleEvent :: IORef Int -> Chan Request -> BrickEvent Name WorkerEvent -> EventM Name Model ()
handleEvent latest requests event = do
  panel <- gets mPanel
  case (event, panel) of
    (AppEvent e, _) -> modify (applyWorker e)
    (VtyEvent (V.EvKey V.KEsc []), FormPanel) -> modify (\m -> m {mPanel = DetailsPanel})
    (_, FormPanel) -> do
      m <- get
      form <- nestEventM' (mForm m) (handleFormEvent event)
      let (m', request) = formEdited m {mForm = form}
      put m'
      -- Publish the new generation even without a request: an emptied form
      -- must still cancel whatever preview is running.
      when (mGen m' /= mGen m) $ liftIO (atomicWriteIORef latest (mGen m'))
      forM_ request $ \(gen, spec) -> liftIO (writeChan requests (RunPreview gen spec))
    (VtyEvent ev, DetailsPanel) -> case keyCommand ev of
      Just Quit -> halt
      Just ToggleRaw -> modify toggleRaw
      Just OpenForm -> modify (\m -> m {mPanel = FormPanel})
      Just (ScrollDetails n) -> vScrollBy (viewportScroll Details) n
      Nothing -> do
        before <- gets (fmap fst . selected)
        m <- get
        commits <- nestEventM' (mCommits m) (handleListEventVi handleListEvent ev)
        put m {mCommits = commits}
        after <- gets (fmap fst . selected)
        when (before /= after) $ vScrollToBeginning (viewportScroll Details)
    _ -> pure ()
