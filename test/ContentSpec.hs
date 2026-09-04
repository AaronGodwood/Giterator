module ContentSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Either (fromRight)
import Fixture
import Git.Rewrite
import Git.Store
import Test.Hspec
import Transform.Content

-- A leaked token added in the first commit and edited in the second, a commit
-- that only touches the secret, and a binary file that happens to contain it.
secretHistory :: [FixtureCommit]
secretHistory =
  [ FixtureCommit [("README.md", "hello\n"), ("config/app.env", "TOKEN=abc123\n")] "add config" 1700000000
  , FixtureCommit [("config/app.env", "TOKEN=abc123\nDEBUG=1\n"), ("src/a.txt", "a\n")] "debug flag" 1700003600
  , FixtureCommit [("config/app.env", "TOKEN=abc123\nDEBUG=0\n")] "turn debug off" 1700007200
  , FixtureCommit [("logo.bin", "\0abc123\0"), ("notes.md", "the token is abc123\n")] "logo and notes" 1700010800
  ]

run :: FilePath -> ContentOptions -> Bool -> IO Report
run repo opts prune =
  withStore repo $ \s -> rewriteBranches s defaultRewriteOptions {roPruneEmpty = prune} (contentPlan opts)

-- | Every version of every file reachable from main, as one blob of text.
allContent :: FilePath -> IO ByteString
allContent repo = git repo ["log", "-p", "--format=", "main"]

filesAt :: FilePath -> String -> IO [ByteString]
filesAt repo rev = BC.lines <$> git repo ["ls-tree", "-r", "--name-only", rev]

literal :: String -> Replacement
literal = fromRight (error "bad replacement") . literalReplacement

spec :: Spec
spec = do
  describe "replaceAll" $ do
    it "replaces every literal occurrence" $
      replaceAll (Literal "ab" "X") "abcabab" `shouldBe` "XcXX"
    it "replaces regex matches" $
      replaceAll (fromRight (error "bad") (regexReplacement "[0-9]+=>N")) "a1b22c" `shouldBe` "aNbNc"
    it "ignores empty regex matches" $
      replaceAll (fromRight (error "bad") (regexReplacement "x*=>-")) "abxc" `shouldBe` "ab-c"
    it "detects binary content like git does" $ do
      isBinary "text\n" `shouldBe` False
      isBinary "\0bin" `shouldBe` True

  describe "globs" $ do
    it "match names at any depth without a slash" $ do
      pathMatches "*.env" "config/app.env" `shouldBe` True
      pathMatches "config" "config" `shouldBe` True
      pathMatches "*.env" "app.envy" `shouldBe` False
    it "match full paths with a slash" $ do
      pathMatches "config/*.env" "config/app.env" `shouldBe` True
      pathMatches "config/*.env" "x/config/app.env" `shouldBe` False
      pathMatches "src/**/*.hs" "src/a/b/C.hs" `shouldBe` True
      pathMatches "src/**/*.hs" "src/C.hs" `shouldBe` True
      pathMatches "src/*.hs" "src/a/C.hs" `shouldBe` False

  describe "rewriting history" $ do
    it "removes a secret from every version of every text file" $
      withFixtureRepo secretHistory $ \repo -> do
        r <- run repo defaultContentOptions {coReplace = [literal "abc123=>REDACTED"]} False
        length (rpChanged r) `shouldBe` 4
        content <- allContent repo
        BC.unpack content `shouldNotContain` "TOKEN=abc123"
        BC.unpack content `shouldContain` "TOKEN=REDACTED"
        _ <- git repo ["fsck", "--strict", "--no-progress"]
        git repo ["status", "--porcelain"] `shouldReturn` ""

    it "leaves binary files alone" $
      withFixtureRepo secretHistory $ \repo -> do
        _ <- run repo defaultContentOptions {coReplace = [literal "abc123=>REDACTED"]} False
        git repo ["cat-file", "blob", "main:logo.bin"] `shouldReturn` "\0abc123\0"

    it "limits replacements to --path globs" $
      withFixtureRepo secretHistory $ \repo -> do
        _ <- run repo defaultContentOptions {coReplace = [literal "abc123=>REDACTED"], coPaths = ["*.md"]} False
        git repo ["cat-file", "blob", "main:notes.md"] `shouldReturn` "the token is REDACTED"
        git repo ["cat-file", "blob", "main:config/app.env"] `shouldReturn` "TOKEN=abc123\nDEBUG=0"

    it "changes nothing when nothing matches" $
      withFixtureRepo secretHistory $ \repo -> do
        r <- run repo defaultContentOptions {coReplace = [literal "not-there=>x"], coDelete = ["nope"]} False
        rpChanged r `shouldBe` []

    it "deletes a file from every commit" $
      withFixtureRepo secretHistory $ \repo -> do
        r <- run repo defaultContentOptions {coDelete = ["*.env"]} False
        rpCommits r `shouldBe` 4
        names <- git repo ["log", "--format=", "--name-only", "main"]
        BC.unpack names `shouldNotContain` "app.env"
        filesAt repo "main" `shouldReturn` ["README.md", "logo.bin", "notes.md", "src/a.txt"]

    it "drops commits left empty when pruning" $
      withFixtureRepo secretHistory $ \repo -> do
        r <- run repo defaultContentOptions {coDelete = ["config"]} True
        length (rpPruned r) `shouldBe` 1
        subjects <- BC.lines <$> git repo ["log", "--format=%s", "main"]
        subjects `shouldBe` ["logo and notes", "debug flag", "add config"]
        _ <- git repo ["fsck", "--strict", "--no-progress"]
        pure ()

    it "renames files and directories in every commit" $
      withFixtureRepo secretHistory $ \repo -> do
        _ <- run repo defaultContentOptions {coRename = [("config", "settings"), ("src/a.txt", "lib/b.txt")]} False
        filesAt repo "main" `shouldReturn` ["README.md", "lib/b.txt", "logo.bin", "notes.md", "settings/app.env"]
        filesAt repo "main~3" `shouldReturn` ["README.md", "settings/app.env"]
        git repo ["cat-file", "blob", "main~2:settings/app.env"] `shouldReturn` "TOKEN=abc123\nDEBUG=1"
        _ <- git repo ["fsck", "--strict", "--no-progress"]
        pure ()

    -- Matters for leaked secrets: rewriting alone does not purge them locally.
    it "keeps the old content reachable through the backup ref" $
      withFixtureRepo secretHistory $ \repo -> do
        _ <- run repo defaultContentOptions {coReplace = [literal "abc123=>REDACTED"]} False
        old <- git repo ["log", "-p", "--format=", "refs/giterator/1/old/heads/main"]
        BC.unpack old `shouldContain` "TOKEN=abc123"
