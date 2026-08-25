module StoreSpec (spec) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import Fixture
import Git.Object (checkRoundTrip)
import Git.Store
import Git.Types
import Test.Hspec

sample :: [FixtureCommit]
sample =
  [ FixtureCommit [("README.md", "hello\n")] "first" 1700000000
  , FixtureCommit [("src/a.txt", "a\n"), ("src/deep/b.bin", "\0\1\2")] "second" 1700003600
  , FixtureCommit [("README.md", "hello again\n")] "third\n\nwith a body" 1700007200
  ]

spec :: Spec
spec = do
  describe "fixture repos" $
    it "are deterministic: same inputs give the same HEAD hash" $ do
      a <- withFixtureRepo sample (\repo -> git repo ["rev-parse", "HEAD"])
      b <- withFixtureRepo sample (\repo -> git repo ["rev-parse", "HEAD"])
      a `shouldBe` b

  describe "Store" $ do
    it "resolves names and parses the commit" $
      withFixtureRepo sample $ \repo -> withStore repo $ \store -> do
        Just (oid, ObjCommit, _) <- lookupObject store "HEAD"
        c <- readCommit store oid
        cMessage c `shouldBe` "third\n\nwith a body\n"
        sigTime (cAuthor c) `shouldBe` 1700007200
        length (cParents c) `shouldBe` 1

    it "returns Nothing for missing objects" $
      withFixtureRepo sample $ \repo -> withStore repo $ \store ->
        lookupObject store "does-not-exist" >>= (`shouldSatisfy` not . isJust)

    it "reads blobs with binary content exactly" $
      withFixtureRepo sample $ \repo -> withStore repo $ \store -> do
        Just (_, ObjBlob, body) <- lookupObject store "HEAD:src/deep/b.bin"
        body `shouldBe` "\0\1\2"

    it "round-trips every commit and tree byte-exactly" $
      withFixtureRepo sample $ \repo -> withStore repo $ \store -> do
        oids <- revList store ["--objects", "--no-object-names", "--all"]
        length oids `shouldSatisfy` (> 3)
        forM_ oids $ \oid -> do
          Just (t, body) <- readObject store oid
          checkRoundTrip oid t body `shouldBe` Nothing
