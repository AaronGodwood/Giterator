module Main (main) where

import Fixture
import Git.Types (oidFromHex)
import Test.Hspec

sample :: [FixtureCommit]
sample =
  [ FixtureCommit [("README.md", "hello\n")] "first" 1700000000
  , FixtureCommit [("src/a.txt", "a\n")] "second" 1700003600
  , FixtureCommit [("README.md", "hello again\n")] "third" 1700007200
  ]

headOf :: FilePath -> IO String
headOf repo = show <$> (git repo ["rev-parse", "HEAD"] >>= maybe (fail "bad oid") pure . oidFromHex)

main :: IO ()
main = hspec $
  describe "fixture repos" $ do
    it "have the expected number of commits" $
      withFixtureRepo sample $ \repo ->
        git repo ["rev-list", "--count", "HEAD"] `shouldReturn` "3"

    it "are deterministic: same inputs give the same HEAD hash" $ do
      a <- withFixtureRepo sample headOf
      b <- withFixtureRepo sample headOf
      a `shouldBe` b
