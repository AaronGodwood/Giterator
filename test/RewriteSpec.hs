module RewriteSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Fixture
import Git.Refs
import Git.Rewrite
import Git.Store
import Git.Types
import Test.Hspec
import Transform.Time (TimeOptions (..), Which (..), defaultTimeOptions, timePlan)

-- main: first - second - third - merge
--                    \           /
-- side:                side work
-- plus a lightweight tag on third and an annotated tag on the merge.
withHistory :: (FilePath -> IO a) -> IO a
withHistory act = withFixtureRepo base $ \repo -> do
  _ <- fixtureGit repo Nothing ["checkout", "-q", "-b", "side", "HEAD~1"]
  fixtureCommit repo (FixtureCommit [("side.txt", "s\n")] "side work" 1700010000)
  _ <- fixtureGit repo Nothing ["checkout", "-q", "main"]
  _ <- fixtureGit repo (Just 1700020000) ["merge", "-q", "--no-ff", "side", "-m", "merge side"]
  _ <- fixtureGit repo Nothing ["tag", "light", "main~1"]
  _ <- fixtureGit repo (Just 1700030000) ["tag", "-a", "annotated", "-m", "v1", "main"]
  act repo
  where
    base =
      [ FixtureCommit [("README.md", "hello\n")] "first" 1700000000
      , FixtureCommit [("src/a.txt", "a\n")] "second" 1700003600
      , FixtureCommit [("README.md", "hello again\n")] "third" 1700007200
      ]

prefixMessages :: Transform
prefixMessages = pureTransform (\c -> c {cMessage = "x: " <> cMessage c})

rewrite :: FilePath -> Bool -> Transform -> [ByteString] -> IO Report
rewrite repo dryRun t = rewriteWith repo dryRun (const (pure t))

rewriteWith :: FilePath -> Bool -> Plan -> [ByteString] -> IO Report
rewriteWith repo dryRun plan branches =
  withStore repo (\s -> rewriteBranches s defaultRewriteOptions {roBranches = branches, roDryRun = dryRun} plan)

dates :: FilePath -> String -> IO [[Int]]
dates repo rev = map (map (read . BC.unpack) . BC.words) . BC.lines <$> git repo ["log", "--format=%at %ct", rev]

refsOf :: FilePath -> IO ByteString
refsOf repo = git repo ["for-each-ref", "--format=%(objectname) %(refname)"]

subjects :: FilePath -> String -> IO [ByteString]
subjects repo rev = BC.lines <$> git repo ["log", "--format=%s", rev]

spec :: Spec
spec = do
  describe "identity rewrite" $
    it "reproduces every hash and touches no refs" $ withHistory $ \repo -> do
      original <- refsOf repo
      r <- rewrite repo False mempty []
      rpCommits r `shouldBe` 5
      rpChanged r `shouldBe` []
      rpBackup r `shouldBe` Nothing
      refsOf repo `shouldReturn` original

  describe "rewriting messages" $ do
    it "rewrites every commit, including through the merge" $ withHistory $ \repo -> do
      r <- rewrite repo False prefixMessages []
      length (rpChanged r) `shouldBe` 5
      subjects repo "main" >>= (`shouldSatisfy` all ("x: " `BS.isPrefixOf`))
      mergeSecondParent <- git repo ["rev-parse", "main^2"]
      git repo ["rev-parse", "side"] `shouldReturn` mergeSecondParent
      _ <- git repo ["fsck", "--strict", "--no-progress"]
      git repo ["status", "--porcelain"] `shouldReturn` ""

    it "moves branches and lightweight tags but reports annotated tags" $ withHistory $ \repo -> do
      r <- rewrite repo False prefixMessages []
      map ruRef (rpUpdates r) `shouldMatchList` ["refs/heads/main", "refs/heads/side", "refs/tags/light"]
      map refName (rpSkippedTags r) `shouldBe` ["refs/tags/annotated"]
      rpBackup r `shouldBe` Just 1

    it "only rewrites the branches asked for" $ withHistory $ \repo -> do
      mainBefore <- git repo ["rev-parse", "main"]
      r <- rewrite repo False prefixMessages ["side"]
      rpCommits r `shouldBe` 3
      map ruRef (rpUpdates r) `shouldBe` ["refs/heads/side"]
      git repo ["rev-parse", "main"] `shouldReturn` mainBefore

    it "leaves refs alone on a dry run" $ withHistory $ \repo -> do
      original <- refsOf repo
      r <- rewrite repo True prefixMessages []
      length (rpChanged r) `shouldBe` 5
      rpBackup r `shouldBe` Nothing
      refsOf repo `shouldReturn` original

    it "refuses to run with uncommitted changes" $ withHistory $ \repo -> do
      BS.writeFile (repo <> "/README.md") "edited\n"
      rewrite repo False prefixMessages [] `shouldThrow` anyIOException

  describe "undo" $ do
    it "restores every ref and removes the backup" $ withHistory $ \repo -> do
      original <- refsOf repo
      _ <- rewrite repo False prefixMessages []
      _ <- rewrite repo False prefixMessages []
      Just (2, _) <- withStore repo undoLatest
      Just (1, _) <- withStore repo undoLatest
      refsOf repo `shouldReturn` original
      withStore repo undoLatest >>= (`shouldSatisfy` null)
      git repo ["status", "--porcelain"] `shouldReturn` ""

    it "refuses if a branch moved since the rewrite" $ withHistory $ \repo -> do
      _ <- rewrite repo False prefixMessages []
      fixtureCommit repo (FixtureCommit [] "later work" 1700040000)
      tip <- git repo ["rev-parse", "main"]
      withStore repo undoLatest `shouldThrow` anyException
      git repo ["rev-parse", "main"] `shouldReturn` tip

  describe "rewriting dates" $ do
    it "shifts author and committer dates" $ withHistory $ \repo -> do
      original <- dates repo "main"
      _ <- rewriteWith repo False (timePlan defaultTimeOptions {toShift = Just 86400}) []
      dates repo "main" `shouldReturn` map (map (+ 86400)) original

    it "spreads history across a range, keeping the ends" $ withHistory $ \repo -> do
      let (from, to) = (1704067200, 1709251200)
      _ <- rewriteWith repo False (timePlan defaultTimeOptions {toSpread = Just (from, to), toWhich = Author}) []
      authored <- map head <$> dates repo "main"
      maximum authored `shouldBe` fromIntegral to
      minimum authored `shouldBe` fromIntegral from

    it "warns about commits newly dated before their parent" $ withHistory $ \repo -> do
      let backdateThird = pureTransform $ \c ->
            if cMessage c == "third\n" then c {cCommitter = (cCommitter c) {sigTime = 0}} else c
      r <- rewrite repo False backdateThird []
      length (rpOutOfOrder r) `shouldBe` 1
      r' <- rewrite repo False prefixMessages []
      rpOutOfOrder r' `shouldBe` []

  describe "written objects" $
    it "are readable by the same store that wrote them" $ withHistory $ \repo -> withStore repo $ \s -> do
      _ <- rewriteBranches s defaultRewriteOptions (const (pure prefixMessages))
      Just (oid, ObjCommit, _) <- lookupObject s "main"
      c <- readCommit s oid
      cMessage c `shouldBe` "x: merge side\n"
