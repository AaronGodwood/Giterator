module Main (main) where

import Control.Monad (foldM, forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Git.Date (formatSignatureDate)
import Git.Object
import Git.Refs
import Git.Rewrite
import Git.Store
import Git.Types
import Numeric (showHex)
import Options.Applicative
import System.Exit (die, exitFailure)
import System.IO (hSetBinaryMode, stdout)
import Transform.Content
import Transform.Time

data Command
  = Log (Maybe String) (Maybe Int)
  | Show String Bool
  | Verify (Maybe String)
  | Rewrite RewriteOptions TimeOptions ContentOptions
  | Undo

data Options = Options
  { optRepo    :: FilePath
  , optCommand :: Command
  }

main :: IO ()
main = do
  opts <- execParser cli
  -- Names and messages are arbitrary bytes; writing them as-is avoids Windows codepage errors.
  hSetBinaryMode stdout True
  withStore (optRepo opts) $ \store -> case optCommand opts of
    Log rev limit -> runLog store rev limit
    Show rev raw  -> runShow store rev raw
    Verify rev    -> runVerify store rev
    Rewrite ro to co -> runRewrite store ro (contentPlan co <> timePlan to)
    Undo          -> runUndo store

cli :: ParserInfo Options
cli = info (options <**> helper) (fullDesc <> progDesc "Inspect and rewrite git history")
  where
    options =
      Options
        <$> strOption (short 'C' <> metavar "PATH" <> value "." <> showDefault <> help "Repository to operate on")
        <*> hsubparser (logCmd <> showCmd <> verifyCmd <> rewriteCmd <> undoCmd)
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
            <> "date options apply in the order: spread, shift, jitter, work-hours, tz"
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
    revArg = strArgument (metavar "REV")
    limitOpt = option auto (short 'n' <> metavar "N" <> help "Show at most N commits")

-- log ------------------------------------------------------------------------

runLog :: Store -> Maybe String -> Maybe Int -> IO ()
runLog store rev limit = do
  oids <- revList store $
    ["--topo-order"] <> foldMap (\n -> ["-n", show n]) limit <> ["--end-of-options", fromMaybe "HEAD" rev]
  forM_ oids $ \oid -> readCommit store oid >>= BC.putStr . logEntry oid

logEntry :: Oid -> Commit -> ByteString
logEntry oid c = BS.concat [headline, committedLine]
  where
    a = cAuthor c
    k = cCommitter c
    mark = if length (cParents c) > 1 then " M " else "   "
    subject = BC.takeWhile (/= '\n') (cMessage c)
    headline = shortHex oid <> mark <> formatSignatureDate a <> "  " <> sigName a <> "  " <> subject <> "\n"
    sameWho = (sigName a, sigEmail a) == (sigName k, sigEmail k)
    sameWhen = (sigTime a, sigTz a) == (sigTime k, sigTz k)
    committedLine
      | sameWho && sameWhen = ""
      | otherwise =
          BC.replicate 10 ' ' <> "committed " <> formatSignatureDate k
            <> (if sameWho then "" else "  by " <> sigName k) <> "\n"

-- show -----------------------------------------------------------------------

runShow :: Store -> String -> Bool -> IO ()
runShow store rev raw = lookupObject store (BC.pack rev) >>= \case
  Nothing -> die ("giterator: unknown object " <> rev)
  Just (oid, t, body) -> BC.putStr ((if raw then showRaw else showPretty) oid t body)

showPretty :: Oid -> ObjectType -> ByteString -> ByteString
showPretty oid t body = case t of
  ObjCommit | Just c <- parseCommit body -> prettyCommit oid c
  ObjTree | Just tree <- parseTree body -> foldMap prettyEntry tree
  _ -> body

prettyCommit :: Oid -> Commit -> ByteString
prettyCommit oid c =
  BS.concat $
    [field "commit" (oidToHex oid), field "tree" (oidToHex (cTree c))]
      <> map (field "parent" . oidToHex) (cParents c)
      <> [field "author" (sig (cAuthor c)), field "committer" (sig (cCommitter c))]
      <> [field k (summarise v) | (k, v) <- cExtra c]
      <> ["\n"]
      <> map (\l -> "    " <> l <> "\n") (BC.lines (cMessage c))
  where
    field k v = k <> BC.replicate (max 1 (10 - BS.length k)) ' ' <> v <> "\n"
    sig s = sigName s <> " <" <> sigEmail s <> ">  " <> formatSignatureDate s <> "  (unix " <> BC.pack (show (sigTime s)) <> ")"
    summarise v = case BC.lines v of
      [l] -> l
      ls -> "(" <> BC.pack (show (length ls)) <> " lines)"

prettyEntry :: TreeEntry -> ByteString
prettyEntry e = teMode e <> " " <> kind <> " " <> oidToHex (teOid e) <> "\t" <> teName e <> "\n"
  where
    kind = case teMode e of
      m | m `elem` ["40000", "040000"] -> "tree  "
      "160000" -> "commit"
      _ -> "blob  "

showRaw :: Oid -> ObjectType -> ByteString -> ByteString
showRaw oid t body =
  BS.concat
    [ "type:    ", objectTypeName t, "\n"
    , "size:    ", BC.pack (show (BS.length body)), " bytes\n"
    , "hashed:  sha1(\"", escape (objectHeader t body), "\" <> body)\n"
    , "result:  ", oidToHex computed, verdict, "\n"
    , "---\n"
    , rawBody
    ]
  where
    computed = hashObject t body
    verdict
      | computed == oid = "  (matches git)"
      | otherwise = "  (MISMATCH: git says " <> oidToHex oid <> ")"
    rawBody = case t of
      ObjTree | Just tree <- parseTree body ->
        "(one entry per line for readability; the real bytes have no newlines)\n"
          <> foldMap (\e -> escape (teMode e <> " " <> teName e <> "\0") <> escape (oidToRaw (teOid e)) <> "\n") tree
      _ -> body <> (if "\n" `BS.isSuffixOf` body then "" else "\n")

-- | Printable ASCII and newlines as-is; everything else as @\\0@ or @\\xHH@.
escape :: ByteString -> ByteString
escape = BS.concatMap $ \w -> case w of
  0 -> "\\0"
  92 -> "\\\\"
  10 -> "\n"
  _ | w >= 0x20 && w < 0x7f -> BS.singleton w
    | otherwise -> "\\x" <> BC.pack (pad (showHex w ""))
  where
    pad s = replicate (2 - length s) '0' <> s

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
