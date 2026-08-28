-- | Throwaway git repos with pinned identities and dates, so their hashes are deterministic.
module Fixture
  ( FixtureCommit (..)
  , withFixtureRepo
  , fixtureCommit
  , fixtureGit
  , git
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (createTempDirectory)
import System.Process.Typed (proc, readProcessStdout_, setEnv, setWorkingDir)

data FixtureCommit = FixtureCommit
  { fcFiles   :: [(FilePath, ByteString)]
  , fcMessage :: String
  , fcTime    :: Integer
  }

-- | 'removePathForcibly' rather than temporary's own cleanup: git marks objects
-- read-only, which makes the default removal fail silently on Windows.
withFixtureRepo :: [FixtureCommit] -> (FilePath -> IO a) -> IO a
withFixtureRepo commits action =
  bracket acquire removePathForcibly $ \root -> do
    let repo = root </> "repo"
    createDirectoryIfMissing True repo
    BS.writeFile (root </> "empty-gitconfig") BS.empty
    _ <- fixtureGit repo Nothing ["init", "-q", "-b", "main"]
    _ <- fixtureGit repo Nothing ["config", "core.autocrlf", "false"]
    mapM_ (fixtureCommit repo) commits
    action repo
  where
    acquire = getTemporaryDirectory >>= (`createTempDirectory` "giterator")

-- | Run git with the fixture's isolated config, optionally pinning both dates.
fixtureGit :: FilePath -> Maybe Integer -> [String] -> IO ByteString
fixtureGit repo = gitWith (isolatedEnv (takeDirectory repo)) repo

fixtureCommit :: FilePath -> FixtureCommit -> IO ()
fixtureCommit repo c = do
  mapM_ writeFixtureFile (fcFiles c)
  _ <- fixtureGit repo Nothing ["add", "-A"]
  _ <- fixtureGit repo (Just (fcTime c)) ["commit", "-q", "--allow-empty", "-m", fcMessage c]
  pure ()
  where
    writeFixtureFile (path, contents) = do
      let full = repo </> path
      createDirectoryIfMissing True (takeDirectory full)
      BS.writeFile full contents

-- | Keeps the user's global/system config (signing, hooks, autocrlf) out of fixtures.
isolatedEnv :: FilePath -> [(String, String)]
isolatedEnv root =
  [ ("GIT_CONFIG_NOSYSTEM", "1")
  , ("GIT_CONFIG_GLOBAL", root </> "empty-gitconfig")
  , ("GIT_AUTHOR_NAME", "Fixture Author")
  , ("GIT_AUTHOR_EMAIL", "author@example.com")
  , ("GIT_COMMITTER_NAME", "Fixture Committer")
  , ("GIT_COMMITTER_EMAIL", "committer@example.com")
  ]

gitWith :: [(String, String)] -> FilePath -> Maybe Integer -> [String] -> IO ByteString
gitWith extra repo time args = do
  inherited <- getEnvironment
  let dates = case time of
        Nothing -> []
        Just t  -> let d = show t <> " +0000" in [("GIT_AUTHOR_DATE", d), ("GIT_COMMITTER_DATE", d)]
      overrides = extra <> dates
      env = overrides <> filter ((`notElem` map fst overrides) . fst) inherited
  BL.toStrict <$> readProcessStdout_ (setWorkingDir repo . setEnv env $ proc "git" args)

-- | Run git in a fixture repo and return trimmed stdout.
git :: FilePath -> [String] -> IO ByteString
git repo args =
  BS.dropWhileEnd (`elem` [10, 13]) . BL.toStrict
    <$> readProcessStdout_ (setWorkingDir repo (proc "git" args))
