-- | Wiring: brick on the main thread, and one worker thread that owns the
-- 'Store' (its cat-file pipe can't be shared) and does all git work, so the
-- screen stays responsive while histories load, previews run and rewrites apply.
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
import Data.Text qualified as T
import Git.Refs (Ref (..), currentBranch, resolveBranch, undoLatest)
import Git.Rewrite (RewriteOptions (..), defaultRewriteOptions, rewriteBranches)
import Git.Store
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Tui.Form (PreviewSpec (..), specPlan)
import Tui.Model
import Tui.Preview
import Tui.View

data Request
  = LoadHistory
  | RunPreview Int PreviewSpec
  | RunApply PreviewSpec
  | RunUndo

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
  writeChan requests LoadHistory
  bracket (forkIO (worker store branch latest requests events)) killThread $ \_ -> do
    let buildVty = mkVty V.defaultConfig
    vty <- buildVty
    void (customMain vty buildVty (Just events) (app latest requests) (initialModel branch))

worker :: Store -> ByteString -> IORef Int -> Chan Request -> BChan WorkerEvent -> IO ()
worker store branch latest requests events = load >>= loop
  where
    send = writeBChan events
    failure (e :: SomeException) = displayException e

    -- Oldest first, as the rewrite engine wants it; the UI gets newest first.
    load = do
      result <- try $ do
        oids <- revList store ["--topo-order", "--end-of-options", BC.unpack branch]
        traverse (\o -> (o,) <$> readCommit store o) oids
      case result of
        Right commits -> reverse commits <$ send (HistoryLoaded commits)
        Left e -> [] <$ send (WorkerFailed (failure e))

    loop history =
      readChan requests >>= \case
        LoadHistory -> load >>= loop
        RunPreview gen spec -> do
          let superseded = (/= gen) <$> readIORef latest
          result <- try (previewRewrite store superseded (psPrune spec) (specPlan spec) history)
          case result of
            Right preview -> send (PreviewDone gen (Right preview))
            Left e
              | Just PreviewCancelled <- fromException e -> pure ()
              | otherwise -> send (PreviewDone gen (Left (failure e)))
          loop history
        RunApply spec -> do
          let opts = defaultRewriteOptions {roBranches = [branch], roPruneEmpty = psPrune spec}
          result <- try (rewriteBranches store opts (specPlan spec))
          send (Applied (either (Left . failure) (Right . applySummary) result))
          load >>= loop
        RunUndo -> do
          result <- try (undoLatest store)
          send . Undone $ case result of
            Left e -> Left (failure e)
            Right Nothing -> Left "no rewrites to undo"
            Right (Just (n, _)) -> Right ("restored backup #" <> T.pack (show n))
          load >>= loop

app :: IORef Int -> Chan Request -> App Model WorkerEvent Name
app latest requests =
  App
    { appDraw = draw
    , appChooseCursor = \m -> case mPanel m of
        FormPanel -> focusRingCursor formFocus (mForm m)
        -- Only the search box asks for a cursor here.
        DetailsPanel -> showFirstCursor m
    , appHandleEvent = handleEvent latest requests
    , appStartEvent = pure ()
    , appAttrMap = const attributes
    }

handleEvent :: IORef Int -> Chan Request -> BrickEvent Name WorkerEvent -> EventM Name Model ()
handleEvent latest requests event = do
  m <- get
  case (event, mOverlay m) of
    (AppEvent e, _) -> modify (applyWorker e)
    (VtyEvent _, HelpOverlay) -> put m {mOverlay = NoOverlay}
    (VtyEvent ev, ConfirmOverlay action) -> case confirmKey ev of
      Just True -> do
        -- A new generation cancels any preview still running before the rewrite starts.
        let gen = mGen m + 1
        put m {mOverlay = NoOverlay, mBusy = True, mGen = gen, mMessage = Nothing}
        liftIO $ do
          atomicWriteIORef latest gen
          writeChan requests $ case action of
            ApplyRewrite spec -> RunApply spec
            UndoRewrite -> RunUndo
      Just False -> put m {mOverlay = NoOverlay}
      Nothing -> pure ()
    (VtyEvent ev, NoOverlay)
      | mSearching m -> put (searchKey ev m)
      | mPanel m == FormPanel -> formEvent ev
      | otherwise -> listEvent ev
    _ -> pure ()
  where
    formEvent = \case
      V.EvKey V.KEsc [] -> modify (\m -> m {mPanel = DetailsPanel})
      _ -> do
        m <- get
        form <- nestEventM' (mForm m) (handleFormEvent event)
        let (m', request) = formEdited m {mForm = form}
        put m'
        -- Publish the new generation even without a request: an emptied form
        -- must still cancel whatever preview is running.
        when (mGen m' /= mGen m) $ liftIO (atomicWriteIORef latest (mGen m'))
        forM_ request $ \(gen, spec) -> liftIO (writeChan requests (RunPreview gen spec))

    listEvent ev = do
      m <- get
      case keyCommand ev of
        Just Quit -> halt
        Just ToggleRaw -> put (toggleRaw m)
        Just ToggleDates -> put (toggleDates m)
        Just OpenForm -> put m {mPanel = FormPanel}
        Just StartSearch -> put m {mSearching = True}
        Just ShowHelp -> put m {mOverlay = HelpOverlay}
        Just Apply | not (mBusy m) -> put (requestApply m)
        Just Undo | not (mBusy m) -> put (requestUndo m)
        Just (ScrollDetails n) -> vScrollBy (viewportScroll Details) n
        Just _ -> pure ()
        Nothing -> do
          commits <- nestEventM' (mCommits m) (handleListEventVi handleListEvent ev)
          put m {mCommits = commits}
          when (fmap fst (selected m) /= fmap fst (selected m {mCommits = commits})) $
            vScrollToBeginning (viewportScroll Details)
