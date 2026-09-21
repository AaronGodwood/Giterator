module Main (main) where

import Control.Monad (foldM, forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Git.Date (formatSignatureDate)
import Git.Object
import Git.Pretty
import Git.Refs
import Git.Rewrite
import Git.Store
import Git.Types
import Options.Applicative
import System.Exit (die, exitFailure)
import System.IO (hFlush, hSetBinaryMode, stdout)
import Transform.Content
import Transform.Time
import Transform.Vanity
import Tui.Run (runTui)

data Command
  = Log (Maybe String) (Maybe Int)
  | Show String Bool
  | Verify (Maybe String)
  | Rewrite RewriteOptions TimeOptions ContentOptions
  | Undo
  | Purge Bool
  | Vanity RewriteOptions VanityOptions
  | Tui (Maybe ByteString)

data Options = Options
  { optRepo    :: FilePath
  , optCommand :: Command
  }

main :: IO ()
main = do
  opts <- execParser cli
  let repo = optRepo opts
      -- Names and messages are arbitrary bytes; writing them as-is avoids Windows
      -- codepage errors. Not for the TUI: vty draws through the normal text handle.
      bytes act = hSetBinaryMode stdout True >> act
  case optCommand opts of
    Tui branch       -> runTui repo branch
    Log rev limit    -> bytes $ withStore repo (\s -> runLog s rev limit)
    Show rev raw     -> bytes $ withStore repo (\s -> runShow s rev raw)
    Verify rev       -> bytes $ withStore repo (`runVerify` rev)
    Rewrite ro to co -> bytes $ withStore repo (\s -> runRewrite s ro (contentPlan co <> timePlan to))
    Undo             -> bytes $ withStore repo runUndo
    Purge confirmed  -> bytes $ runPurge repo confirmed
    Vanity ro vo     -> bytes $ withStore repo (\s -> runVanity s ro vo)

cli :: ParserInfo Options
cli = info (options <**> helper) (fullDesc <> progDesc "Inspect and rewrite git history")
  where
    options =
      Options
        <$> strOption (short 'C' <> metavar "PATH" <> value "." <> showDefault <> help "Repository to operate on")
        <*> hsubparser (tuiCmd <> logCmd <> showCmd <> verifyCmd <> rewriteCmd <> vanityCmd <> undoCmd <> purgeCmd)
    tuiCmd =
      command "tui" . info (Tui <$> optional (utf8 <$> strArgument (metavar "BRANCH" <> help "Default: the current branch"))) $
        progDesc "Browse history interactively"
    logCmd =
      command "log" . info (Log <$> optional revArg <*> optional limitOpt) $
        progDesc "List commits, showing committer date too when it differs from the author's"
    showCmd =
      command "show" . info (Show <$> revArg <*> switch (long "raw" <> help "Exact object bytes and how the hash is computed")) $
        progDesc "Show one object (commit, tree, blob or tag)"
    verifyCmd =
      command "verify" . info (Verify <$> optional revArg) $
        progDesc "Check every commit and tree round-trips byte-exactly (default: all refs)"
    rewriteCmd =
      command "rewrite" . info (Rewrite <$> rewriteOpts <*> timeOpts <*> contentOpts) $
        progDesc $
          "Rewrite history of the given branches (default: all). Content changes apply before date changes; "
            <> "date options apply in the order: spread, shift, jitter, weekdays, work-hours, tz"
    rewriteOpts =
      RewriteOptions
        <$> many (utf8 <$> strArgument (metavar "BRANCH..."))
        <*> switch (long "dry-run" <> help "Show what would change without moving any refs")
        <*> switch (long "prune-empty" <> help "Drop commits that the rewrite leaves with no changes")
    contentOpts =
      ContentOptions
        <$> ((<>)
              <$> many (option (eitherReader literalReplacement) (long "replace" <> metavar "OLD=>NEW" <> help "Replace text in every version of every text file"))
              <*> many (option (eitherReader regexReplacement) (long "replace-regex" <> metavar "REGEX=>NEW" <> help "Like --replace, with a POSIX regex (applied after the literal replacements)")))
        <*> many (utf8 <$> strOption (long "path" <> metavar "GLOB" <> help "Only apply replacements to matching files (e.g. '*.env', 'src/**/*.hs')"))
        <*> many (utf8 <$> strOption (long "delete" <> metavar "GLOB" <> help "Remove matching files or directories from every commit"))
        <*> many (option (eitherReader parseArrow) (long "rename" <> metavar "FROM=>TO" <> help "Move a file or directory in every commit"))
    timeOpts =
      TimeOptions
        <$> option (eitherReader parseWhich) (long "dates" <> metavar "author|committer|both" <> value Both <> help "Which dates to change (default: both)")
        <*> optional (option (eitherReader parseRange) (long "spread" <> metavar "FROM..TO" <> help "Stretch or squash history linearly into this range, UTC (e.g. 2024-01-01..2024-03-01)"))
        <*> optional (option (eitherReader parseDuration) (long "shift" <> metavar "DURATION" <> help "Move every date by DURATION (e.g. 3d, -2h30m)"))
        <*> optional (option (eitherReader parseDuration) (long "jitter" <> metavar "DURATION" <> help "Move each commit by a reproducible random offset of up to ±DURATION"))
        <*> (BC.pack <$> strOption (long "seed" <> metavar "TEXT" <> value "giterator" <> help "Seed for --jitter"))
        <*> switch (long "weekdays" <> help "Keep commits off weekends: Friday to Sunday is squeezed into Friday, Monday to Thursday are untouched")
        <*> optional (option (eitherReader parseWindow) (long "work-hours" <> metavar "HH:MM-HH:MM" <> help "Squeeze each day's commits into these local hours"))
        <*> ((\tz mode -> (,mode) <$> tz)
              <$> optional (option (eitherReader parseTz) (long "tz" <> metavar "+HHMM" <> help "Set the timezone (keeps the instant, so the local clock time changes)"))
              <*> flag KeepInstant KeepWallClock (long "keep-wall-clock" <> help "With --tz: keep the local clock time and move the instant instead"))
    parseWhich = \case
      "author" -> Right Author
      "committer" -> Right Committer
      "both" -> Right Both
      s -> Left ("expected author, committer or both, not " <> show s)
    undoCmd =
      command "undo" . info (pure Undo) $
        progDesc "Restore the branches and tags from before the most recent rewrite"
    vanityCmd =
      command "vanity" . info (Vanity <$> vanityRewriteOpts <*> vanityOpts) $
        progDesc "Give branch tips (or with --chain, every commit) an id starting with a chosen hex prefix"
    vanityRewriteOpts =
      RewriteOptions
        <$> many (utf8 <$> strArgument (metavar "BRANCH..." <> help "Default: the current branch"))
        <*> switch (long "dry-run" <> help "Show what would change without moving any refs")
        <*> pure False
    vanityOpts =
      VanityOptions
        <$> option (eitherReader parseTarget) (long "prefix" <> metavar "HEX" <> help "Wanted start of the commit id, e.g. cafe")
        <*> option (eitherReader parseMethod) (long "method" <> metavar "whitespace|seconds" <> value Whitespace <> help "Hide the search in trailing whitespace in the message (default), or in the timestamps' seconds")
        <*> switch (long "chain" <> help "Mine every commit in the rewritten history, not just the tips")
    parseMethod = \case
      "whitespace" -> Right Whitespace
      "seconds" -> Right Seconds
      s -> Left ("expected whitespace or seconds, not " <> show s)
    purgeCmd =
      command "purge" . info (Purge <$> switch (long "yes" <> help "Confirm: this cannot be undone")) $
        progDesc "Permanently delete old history: all backups (undo stops working), all reflogs, and unreachable objects"
    revArg = strArgument (metavar "REV")
    limitOpt = option auto (short 'n' <> metavar "N" <> help "Show at most N commits")

-- log ------------------------------------------------------------------------

runLog :: Store -> Maybe String -> Maybe Int -> IO ()
runLog store rev limit = do
  oids <- revList store $
    ["--topo-order"] <> foldMap (\n -> ["-n", show n]) limit <> ["--end-of-options", fromMaybe "HEAD" rev]
  forM_ oids $ \oid -> readCommit store oid >>= BC.putStr . logEntry oid

-- show -----------------------------------------------------------------------

runShow :: Store -> String -> Bool -> IO ()
runShow store rev raw = lookupObject store (BC.pack rev) >>= \case
  Nothing -> die ("giterator: unknown object " <> rev)
  Just (oid, t, body) -> BC.putStr ((if raw then showRaw else showPretty) oid t body)

-- rewrite / undo -------------------------------------------------------------

runRewrite :: Store -> RewriteOptions -> Plan -> IO ()
runRewrite store opts plan = do
  r <- rewriteBranches store opts plan
  let dryRun = roDryRun opts
  BC.putStrLn $
    "rewrote " <> BC.pack (show (rpCommits r)) <> " commits, "
      <> BC.pack (show (length (rpChanged r))) <> " changed"
      <> (if null (rpPruned r) then "" else ", " <> BC.pack (show (length (rpPruned r))) <> " dropped as empty")
  when dryRun $ forM_ (rpChanged r) $ \(old, new) -> do
    before <- readCommit store old
    after <- readCommit store new
    let dates
          | old `elem` rpPruned r = "(dropped)  "
          | cAuthor before == cAuthor after = ""
          | otherwise = formatSignatureDate (cAuthor before) <> " -> " <> formatSignatureDate (cAuthor after) <> "  "
    BC.putStrLn ("  " <> shortHex old <> " -> " <> shortHex new <> "  " <> dates <> BC.takeWhile (/= '\n') (cMessage before))
  unless (null (rpOutOfOrder r)) $
    BC.putStrLn $
      "warning: " <> BC.pack (show (length (rpOutOfOrder r)))
        <> " commit(s) now have a committer date before their parent's; date-sorted views such as GitHub will show them out of order"
  unless (null (rpUnsigned r)) $
    BC.putStrLn $
      "warning: " <> BC.pack (show (length (rpUnsigned r)))
        <> " signed commit(s) changed, so their signatures were removed (they would no longer verify)"
  forM_ (rpUpdates r) $ \u ->
    BC.putStrLn ("  " <> ruRef u <> "  " <> shortHex (ruOld u) <> " -> " <> shortHex (ruNew u))
  forM_ (rpSkippedTags r) $ \t ->
    BC.putStrLn ("warning: annotated tag " <> refName t <> " still points at the old history")
  BC.putStrLn $ case rpBackup r of
    _ | dryRun -> "dry run: no refs were changed"
    Nothing -> "nothing changed"
    Just n -> "backup saved as #" <> BC.pack (show n) <> "; run `giterator undo` to restore"

runUndo :: Store -> IO ()
runUndo store = undoLatest store >>= \case
  Nothing -> BC.putStrLn "no rewrites to undo"
  Just (n, restores) -> do
    forM_ restores $ \u ->
      BC.putStrLn ("  " <> ruRef u <> "  " <> shortHex (ruOld u) <> " -> " <> shortHex (ruNew u))
    BC.putStrLn ("restored backup #" <> BC.pack (show n))

runVanity :: Store -> RewriteOptions -> VanityOptions -> IO ()
runVanity store opts vo = do
  branches <-
    if null (roBranches opts)
      then maybe (die "giterator: HEAD is detached; name a branch to mine") (pure . pure) =<< currentBranch store
      else pure (roBranches opts)
  runRewrite store opts {roBranches = branches} (vanityPlan vo reportMined)
  where
    reportMined old new tries = do
      BC.putStrLn ("  mined " <> shortHex old <> " -> " <> oidToHex new <> "  (" <> BC.pack (show tries) <> " attempts)")
      hFlush stdout

runPurge :: FilePath -> Bool -> IO ()
runPurge repo confirmed = do
  backups <- withStore repo listBackups
  let described = BC.pack (show (length backups)) <> " backup(s)"
  unless confirmed . die . BC.unpack $
    "giterator: purge would permanently delete " <> described
      <> " (undo will stop working), expire every reflog and prune all unreachable objects.\n"
      <> "Rerun with --yes to go ahead."
  withStore repo deleteBackups
  purgeUnreachable repo
  BC.putStrLn ("deleted " <> described <> ", expired reflogs and pruned unreachable objects")
  BC.putStrLn "note: remotes and other clones still have the old history; rewritten branches need a force-push"

-- verify ---------------------------------------------------------------------

runVerify :: Store -> Maybe String -> IO ()
runVerify store rev = do
  oids <- revList store $
    ["--objects", "--no-object-names", "--filter=blob:none"]
      <> maybe ["--all"] (\r -> ["--end-of-options", r]) rev
  (commits, trees, failures) <- foldM step (0 :: Int, 0 :: Int, []) oids
  unless (null failures) $ do
    forM_ (reverse failures) $ \(oid, problem) -> BC.putStrLn (oidToHex oid <> "  " <> BC.pack problem)
    BC.putStrLn (BC.pack (show (length failures)) <> " object(s) failed")
    exitFailure
  BC.putStrLn $
    "verified " <> BC.pack (show commits) <> " commits and " <> BC.pack (show trees)
      <> " trees: all round-trip byte-exactly"
  where
    step (!nc, !nt, fails) oid = readObject store oid >>= \case
      Nothing -> pure (nc, nt, (oid, "could not read object") : fails)
      Just (t, body) ->
        let fails' = maybe fails (\e -> (oid, e) : fails) (checkRoundTrip oid t body)
         in pure $ case t of
              ObjCommit -> (nc + 1, nt, fails')
              ObjTree -> (nc, nt + 1, fails')
              _ -> (nc, nt, fails')
