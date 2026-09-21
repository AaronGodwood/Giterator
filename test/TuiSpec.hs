module TuiSpec (spec) where

import Brick.Widgets.List (listSelected)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Git.Object (hashObject, renderCommit)
import Git.Types
import Graphics.Vty qualified as V
import Test.Hspec
import Tui.Model

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

  describe "displayText" $ do
    it "expands tabs and hides control characters" $
      displayText "a\tb\ESCc" `shouldBe` "a    b\xFFFD\&c"
    it "decodes UTF-8 and survives invalid bytes" $
      displayText "caf\xc3\xa9 \xff" `shouldBe` "café \xFFFD"
