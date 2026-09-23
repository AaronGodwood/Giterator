module TuiSpec (spec) where

import Brick.Widgets.List (listSelected)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Git.Object (hashObject, renderCommit)
import Git.Types
import Graphics.Vty qualified as V
import Fixture
import Git.Store
import Test.Hspec
import Transform.Content (ContentOptions (..))
import Transform.Time (TimeOptions (..))
import Tui.Form
import Tui.Model
import Tui.Preview

commit :: ByteString -> Commit
commit msg =
  Commit
    (fromMaybe (error "bad oid") (oidFromHex "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))
    []
    (Signature "A" "a@example.com" 1700000000 "+0100")
    (Signature "C" "c@example.com" 1700000000 "+0100")
    []
    msg

withId :: Commit -> (Oid, Commit)
withId c = (hashObject ObjCommit (renderCommit c), c)

isRight' :: Either [(Name, String)] (Maybe PreviewSpec) -> Maybe () -> Expectation
isRight' result expected = case result of
  Right built -> void built `shouldBe` expected
  Left errors -> expectationFailure (show errors)

loaded :: Model
loaded = applyWorker (HistoryLoaded (map (withId . commit) ["one\n", "two\n"])) (initialModel "refs/heads/main")

spec :: Spec
spec = do
  describe "worker events" $ do
    it "start in the loading state" $
      mStatus (initialModel "refs/heads/main") `shouldBe` Loading
    it "select the newest commit once history arrives" $ do
      mStatus loaded `shouldBe` Ready
      listSelected (mCommits loaded) `shouldBe` Just 0
      fmap (cMessage . snd) (selected loaded) `shouldBe` Just "one\n"
    it "select nothing for an empty history" $
      selected (applyWorker (HistoryLoaded []) (initialModel "refs/heads/main")) `shouldSatisfy` isNothing
    it "record failures" $
      mStatus (applyWorker (WorkerFailed "boom") loaded) `shouldBe` Failed "boom"

  describe "keys" $ do
    it "map to commands" $ do
      keyCommand (V.EvKey (V.KChar 'q') []) `shouldBe` Just Quit
      keyCommand (V.EvKey V.KEsc []) `shouldBe` Just Quit
      keyCommand (V.EvKey (V.KChar 'r') []) `shouldBe` Just ToggleRaw
      keyCommand (V.EvKey (V.KChar 'J') [V.MShift]) `shouldBe` Just (ScrollDetails 1)
    it "leave navigation to the list" $
      keyCommand (V.EvKey V.KDown []) `shouldBe` Nothing

  describe "details" $ do
    let c = withId (commit "subject\n\nbody\n")
    it "toggle between readable and raw views" $
      mRaw (toggleRaw loaded) `shouldBe` True
    it "show the readable view" $
      detailLines False c `shouldSatisfy` any ("author" `T.isPrefixOf`)
    it "show the raw bytes with a hash that matches" $
      detailLines True c `shouldSatisfy` any ("(matches git)" `T.isInfixOf`)
    it "show the branch without refs/heads/" $
      branchLabel loaded `shouldBe` "main"

  describe "form" $ do
    it "asks for no change when blank" $
      isRight' (buildSpec emptyInput) Nothing
    it "builds a spec from valid fields" $
      case buildSpec emptyInput {fiShift = "3d", fiWeekdays = True, fiDelete = "*.env"} of
        Right (Just built) -> do
          toShift (psTime built) `shouldBe` Just 259200
          toWeekdays (psTime built) `shouldBe` True
          coDelete (psContent built) `shouldBe` ["*.env"]
        _ -> expectationFailure "expected a spec"
    it "names the fields that don't parse" $
      either (map fst) (const []) (buildSpec emptyInput {fiShift = "soon", fiTz = "UTC", fiWorkHours = "09:00-17:00"})
        `shouldMatchList` [FShift, FTz]

  describe "preview requests" $ do
    let typed input = formEdited loaded {mForm = mkForm input}
    it "start a new generation when the input changes" $ do
      let (m, request) = typed emptyInput {fiShift = "1d"}
      fmap fst request `shouldBe` Just 1
      mGen m `shouldBe` 1
    it "aren't repeated for the same input" $ do
      let (m, _) = typed emptyInput {fiShift = "1d"}
      fmap fst (snd (formEdited m)) `shouldBe` Nothing
    it "aren't sent while a field is invalid" $ do
      let (m, request) = typed emptyInput {fiShift = "1"}
      fmap fst request `shouldBe` Nothing
      map fst (mErrors m) `shouldBe` [FShift]
    it "ignore results from older generations" $ do
      let (m, _) = typed emptyInput {fiShift = "1d"}
          stale = applyWorker (PreviewDone 0 (Left "old")) m
      case mPreview stale of
        PreviewRunning -> pure ()
        _ -> expectationFailure "stale result was applied"

  describe "previewLines" $
    it "list what changes" $ do
      let old = commit "subject\n"
          new = old {cAuthor = (cAuthor old) {sigTime = 1700086400}}
      previewLines old (Became (fst (withId new)) new)
        `shouldSatisfy` \ls -> any ("author" `T.isPrefixOf`) ls && not (any ("committer" `T.isPrefixOf`) ls)

  describe "previewRewrite" $ do
    let history =
          [ FixtureCommit [("a.txt", "secret\n")] "first" 1700000000
          , FixtureCommit [("b.txt", "b\n")] "second" 1700003600
          ]
        shiftAndScrub = either (error "bad") (fromMaybe (error "empty")) (buildSpec emptyInput {fiShift = "1d", fiReplace = "secret=>x"})
        oldestFirst store = do
          oids <- revList store ["--topo-order", "--reverse", "main"]
          traverse (\o -> (o,) <$> readCommit store o) oids
    it "previews without writing objects or moving refs" $
      withFixtureRepo history $ \repo -> do
        initial <- (,) <$> git repo ["count-objects", "-v"] <*> git repo ["for-each-ref"]
        p <- withStore repo $ \s -> oldestFirst s >>= previewRewrite s (pure False) False (specPlan shiftAndScrub)
        pvChanged p `shouldBe` 2
        final <- (,) <$> git repo ["count-objects", "-v"] <*> git repo ["for-each-ref"]
        final `shouldBe` initial
    it "stops when superseded" $
      withFixtureRepo history $ \repo ->
        withStore repo (\s -> oldestFirst s >>= previewRewrite s (pure True) False (specPlan shiftAndScrub))
          `shouldThrow` (\PreviewCancelled -> True)

  describe "displayText" $ do
    it "expands tabs and hides control characters" $
      displayText "a\tb\ESCc" `shouldBe` "a    b\xFFFD\&c"
    it "decodes UTF-8 and survives invalid bytes" $
      displayText "caf\xc3\xa9 \xff" `shouldBe` "café \xFFFD"
