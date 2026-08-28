-- | Moving refs to rewritten history, with backups that make every rewrite undoable.
--
-- Backups live at @refs/giterator/<n>/old/...@ and @refs/giterator/<n>/new/...@.
-- Being refs, they keep the old history safe from @git gc@; recording the new
-- value too lets undo refuse if a branch has moved since the rewrite.
module Git.Refs
  ( Ref (..)
  , RefUpdate (..)
  , listRefs
  , resolveBranch
  , planRefUpdates
  , applyRefUpdates
  , undoLatest
  , ensureClean
  , syncWorktree
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Git.Store
import Git.Types

data Ref = Ref
  { refName   :: ByteString
  , refTarget :: Oid
  , refPeeled :: Maybe Oid
    -- ^ For annotated tags, the object the tag object points at.
  }
  deriving (Eq, Show)

data RefUpdate = RefUpdate
  { ruRef :: ByteString
  , ruOld :: Oid
  , ruNew :: Oid
  }
  deriving (Eq, Show)

listRefs :: Store -> [String] -> IO [Ref]
listRefs s patterns = do
  out <- runGit s (["for-each-ref", "--format=%(objectname) %(refname) %(*objectname)"] <> patterns)
  traverse parseLine (BC.lines out)
  where
    parseLine l = case BC.words l of
      [o, name] | Just oid <- oidFromHex o -> pure (Ref name oid Nothing)
      [o, name, p] | Just oid <- oidFromHex o, Just peeled <- oidFromHex p -> pure (Ref name oid (Just peeled))
      _ -> fail ("unexpected for-each-ref output: " <> BC.unpack l)

-- | Accepts @main@ or @refs/heads/main@.
resolveBranch :: Store -> ByteString -> IO Ref
resolveBranch s name = do
  let full = if "refs/heads/" `BS.isPrefixOf` name then name else "refs/heads/" <> name
  refs <- listRefs s [BC.unpack full]
  maybe (fail ("not a branch: " <> BC.unpack name)) pure (find ((== full) . refName) refs)

-- | Refs whose commit was rewritten move to the new commit. Annotated tags are
-- returned separately: moving them means rewriting the tag object, which we don't do yet.
planRefUpdates :: Map Oid Oid -> [Ref] -> ([RefUpdate], [Ref])
planRefUpdates mapping = foldr classify ([], [])
  where
    changed o = case Map.lookup o mapping of
      Just new | new /= o -> Just new
      _ -> Nothing
    classify r (updates, skipped) = case refPeeled r of
      Nothing | Just new <- changed (refTarget r) -> (RefUpdate (refName r) (refTarget r) new : updates, skipped)
      Just p | Just _ <- changed p -> (updates, r : skipped)
      _ -> (updates, skipped)

-- | All updates plus their backups in one @update-ref --stdin@ transaction, so
-- either everything moves or nothing does. Returns the backup number.
applyRefUpdates :: Store -> [RefUpdate] -> IO Int
applyRefUpdates s updates = do
  n <- succ . maximum . (0 :) . map fst . mapMaybe parseBackup <$> listRefs s ["refs/giterator/"]
  runGitInput s ["update-ref", "--stdin"] (foldMap (commands n) updates)
  pure n
  where
    commands n u =
      BS.concat
        [ "create ", backupRef n "old" (ruRef u), " ", oidToHex (ruOld u), "\n"
        , "create ", backupRef n "new" (ruRef u), " ", oidToHex (ruNew u), "\n"
        , "update ", ruRef u, " ", oidToHex (ruNew u), " ", oidToHex (ruOld u), "\n"
        ]

backupRef :: Int -> ByteString -> ByteString -> ByteString
backupRef n kind ref = "refs/giterator/" <> BC.pack (show n) <> "/" <> kind <> "/" <> BS.drop 5 ref

-- | @refs/giterator/3/old/heads/main@ -> @(3, ("old", "refs/heads/main"))@
parseBackup :: Ref -> Maybe (Int, ((ByteString, ByteString), Oid))
parseBackup r = do
  rest <- BS.stripPrefix "refs/giterator/" (refName r)
  let (num, r1) = BC.break (== '/') rest
  (n, "") <- BC.readInt num
  let (kind, r2) = BC.break (== '/') (BS.drop 1 r1)
  pure (n, ((kind, "refs/" <> BS.drop 1 r2), refTarget r))

-- | Restore the most recent backup. Each ref must still be where the rewrite
-- left it; otherwise the whole undo is refused and nothing changes.
undoLatest :: Store -> IO (Maybe (Int, [RefUpdate]))
undoLatest s = do
  backups <- mapMaybe parseBackup <$> listRefs s ["refs/giterator/"]
  case map fst backups of
    [] -> pure Nothing
    ns -> do
      ensureClean s
      let n = maximum ns
          entries = Map.fromList [k | (m, k) <- backups, m == n]
          restores =
            [ RefUpdate ref new old
            | ((kind, ref), old) <- Map.toList entries
            , kind == "old"
            , Just new <- [Map.lookup ("new", ref) entries]
            ]
          commands u =
            BS.concat
              [ "update ", ruRef u, " ", oidToHex (ruNew u), " ", oidToHex (ruOld u), "\n"
              , "delete ", backupRef n "old" (ruRef u), " ", oidToHex (ruNew u), "\n"
              , "delete ", backupRef n "new" (ruRef u), " ", oidToHex (ruOld u), "\n"
              ]
      runGitInput s ["update-ref", "--stdin"] (foldMap commands restores)
      syncWorktree s
      pure (Just (n, restores))

isBare :: Store -> IO Bool
isBare s = (== "true") . BC.strip <$> runGit s ["rev-parse", "--is-bare-repository"]

-- | Uncommitted changes to tracked files would be lost by 'syncWorktree'.
ensureClean :: Store -> IO ()
ensureClean s = do
  bare <- isBare s
  unless bare $ do
    status <- runGit s ["status", "--porcelain", "--untracked-files=no"]
    unless (BS.null status) $
      fail "working tree has uncommitted changes; commit or stash them first"

-- | After moving the checked-out branch, make the index and files match it.
-- Safe only because 'ensureClean' ran first.
syncWorktree :: Store -> IO ()
syncWorktree s = do
  bare <- isBare s
  unless bare $ () <$ runGit s ["reset", "--hard", "-q"]
