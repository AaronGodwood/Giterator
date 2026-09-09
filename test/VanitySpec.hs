module VanitySpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Either (fromRight, isLeft)
import Data.Maybe (fromMaybe)
import Fixture
import Git.Object (hashObject, renderCommit)
import Git.Rewrite
import Git.Store
import Git.Types
import Test.Hspec
import Transform.Vanity

target :: String -> Target
target = fromRight (error "bad target") . parseTarget

sample :: Commit
sample =
  Commit
    (fromMaybe (error "bad oid") (oidFromHex "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))
    []
    (Signature "A" "a@example.com" 1700000000 "+0100")
    (Signature "C" "c@example.com" 1700000000 "+0100")
    []
    "hello\n"

idOf :: Commit -> ByteString
idOf = oidToHex . hashObject ObjCommit . renderCommit

mineWith :: (Int -> Commit -> Search) -> String -> IO Commit
mineWith method hex = do
  let search = method (length hex) sample
  n <- mine (target hex) search
  maybe (fail "not found") (pure . sBuild search) n

history :: [FixtureCommit]
history =
  [ FixtureCommit [("a.txt", "a\n")] "first" 1700000000
  , FixtureCommit [("b.txt", "b\n")] "second" 1700003600
  , FixtureCommit [("c.txt", "c\n")] "third" 1700007200
  ]

vanity :: FilePath -> Method -> Bool -> String -> IO Report
vanity repo method chain hex =
  withStore repo $ \s ->
    rewriteBranches s defaultRewriteOptions (vanityPlan (VanityOptions (target hex) method chain) (\_ _ _ -> pure ()))

spec :: Spec
spec = do
  describe "targets" $ do
    it "parse even and odd lengths" $ do
      matchesTarget (target "cafe") (BS.pack [0xca, 0xfe, 0x12]) `shouldBe` True
      matchesTarget (target "caf") (BS.pack [0xca, 0xf3]) `shouldBe` True
      matchesTarget (target "caf") (BS.pack [0xca, 0xe3]) `shouldBe` False
      matchesTarget (target "CAFE") (BS.pack [0xca, 0xfe]) `shouldBe` True
    it "reject bad input" $ do
      parseTarget "" `shouldSatisfy` isLeft
      parseTarget "xyz" `shouldSatisfy` isLeft

  describe "whitespace method" $ do
    it "finds an id with the prefix, only adding invisible whitespace" $ do
      mined <- mineWith whitespaceSearch "abc"
      idOf mined `shouldSatisfy` ("abc" `BS.isPrefixOf`)
      (BS.stripPrefix "hello" (cMessage mined) >>= BS.stripSuffix "\n")
        `shouldSatisfy` maybe False (BC.all (`elem` (" \t" :: String)))

    it "replaces its own padding when mining again" $ do
      once <- mineWith whitespaceSearch "abc"
      let search = whitespaceSearch 3 once
      Just n <- mine (target "def") search
      BS.length (cMessage (sBuild search n)) `shouldBe` BS.length (cMessage once)

  describe "seconds method" $
    it "finds an id with the prefix, only moving dates a little later" $ do
      mined <- mineWith secondsSearch "abc"
      idOf mined `shouldSatisfy` ("abc" `BS.isPrefixOf`)
      cMessage mined `shouldBe` cMessage sample
      let moved s = sigTime (s mined) - sigTime (s sample)
      moved cAuthor `shouldSatisfy` (\d -> d >= 0 && d <= 86400)
      moved cCommitter `shouldSatisfy` (\d -> d >= 0 && d <= 86400)

  describe "mining" $
    it "is reproducible" $ do
      a <- mineWith whitespaceSearch "0f"
      b <- mineWith whitespaceSearch "0f"
      a `shouldBe` b

  describe "on a repository" $ do
    it "mines the branch tip and leaves its ancestors alone" $
      withFixtureRepo history $ \repo -> do
        parent <- git repo ["rev-parse", "main~1"]
        r <- vanity repo Whitespace False "beef"
        length (rpChanged r) `shouldBe` 1
        git repo ["rev-parse", "main"] >>= (`shouldSatisfy` ("beef" `BS.isPrefixOf`))
        git repo ["rev-parse", "main~1"] `shouldReturn` parent
        git repo ["log", "-1", "--format=%s", "main"] `shouldReturn` "third"
        _ <- git repo ["fsck", "--strict", "--no-progress"]
        pure ()

    it "mines every commit with --chain" $
      withFixtureRepo history $ \repo -> do
        _ <- vanity repo Seconds True "00"
        ids <- BC.lines <$> git repo ["rev-list", "main"]
        length ids `shouldBe` 3
        ids `shouldSatisfy` all ("00" `BS.isPrefixOf`)
