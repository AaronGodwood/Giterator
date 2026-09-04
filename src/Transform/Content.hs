{-# LANGUAGE MultiWayIf #-}

-- | Content transforms: find/replace in file contents, deleting paths and
-- renaming paths, across every commit.
--
-- Trees are rewritten recursively and memoised on (path, tree id): most
-- subtrees are identical from one commit to the next, so each distinct one is
-- visited once per rewrite rather than once per commit.
module Transform.Content
  ( ContentOptions (..)
  , Replacement (..)
  , defaultContentOptions
  , contentPlan
  , replaceAll
  , isBinary
  , globMatch
  , pathMatches
  , parseArrow
  , literalReplacement
  , regexReplacement
  , utf8
  ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Git.Object (renderTree)
import Git.Rewrite (Plan, Transform (..))
import Git.Store
import Git.Types
import Text.Regex.TDFA (Regex, getAllMatches, makeRegexM, match)
import Text.Regex.TDFA.ByteString ()

data Replacement
  = Literal ByteString ByteString
  | Pattern Regex ByteString

data ContentOptions = ContentOptions
  { coReplace :: [Replacement]
    -- ^ Applied in order to every text file.
  , coPaths   :: [ByteString]
    -- ^ Globs limiting which files replacements touch; empty means all.
  , coDelete  :: [ByteString]
    -- ^ Globs; a matching directory goes with everything in it.
  , coRename  :: [(ByteString, ByteString)]
    -- ^ Exact paths (files or directories), applied after replacements and deletes.
  }

defaultContentOptions :: ContentOptions
defaultContentOptions = ContentOptions [] [] [] []

data Env = Env
  { envStore :: Store
  , envOpts  :: ContentOptions
  , envTrees :: IORef (Map (ByteString, Oid) (Maybe Oid))
    -- ^ Nothing: the tree ended up empty and its entry is dropped, as git
    -- never stores empty directories itself.
  , envBlobs :: IORef (Map Oid Oid)
  }

contentPlan :: ContentOptions -> Plan
contentPlan o _
  | null (coReplace o) && null (coDelete o) && null (coRename o) = pure mempty
  | otherwise = do
      trees <- newIORef Map.empty
      blobs <- newIORef Map.empty
      pure . Transform $ \_ c -> do
        store <- ask
        liftIO $ do
          let env = Env store o trees blobs
          root <- rewriteTree env "" (cTree c) >>= maybe (writeObject store ObjTree "") pure
          root' <- foldM (renamePath store) root (coRename o)
          pure c {cTree = root'}

memo :: Ord k => IORef (Map k v) -> k -> IO v -> IO v
memo ref k act =
  Map.lookup k <$> readIORef ref >>= \case
    Just v -> pure v
    Nothing -> do
      v <- act
      modifyIORef' ref (Map.insert k v)
      pure v

-- | @prefix@ is the tree's path with a trailing slash, or empty for the root.
rewriteTree :: Env -> ByteString -> Oid -> IO (Maybe Oid)
rewriteTree env prefix oid = memo (envTrees env) (prefix, oid) $ do
  entries <- readTree (envStore env) oid
  entries' <- catMaybes <$> traverse (rewriteEntry env prefix) entries
  if
    | entries' == entries -> pure (Just oid)
    | null entries' -> pure Nothing
    | otherwise -> Just <$> writeObject (envStore env) ObjTree (renderTree entries')

rewriteEntry :: Env -> ByteString -> TreeEntry -> IO (Maybe TreeEntry)
rewriteEntry env prefix e
  | any (`pathMatches` path) (coDelete o) = pure Nothing
  | isTreeMode (teMode e) = fmap withOid <$> rewriteTree env (path <> "/") (teOid e)
  | isFileMode (teMode e) && replacing = Just . withOid <$> rewriteBlob env (teOid e)
  -- Symlinks and submodules are left alone: their "content" is a link target or a commit id.
  | otherwise = pure (Just e)
  where
    o = envOpts env
    path = prefix <> teName e
    withOid new = e {teOid = new}
    replacing = not (null (coReplace o)) && (null (coPaths o) || any (`pathMatches` path) (coPaths o))

rewriteBlob :: Env -> Oid -> IO Oid
rewriteBlob env oid = memo (envBlobs env) oid $ do
  body <- maybe (fail ("missing blob " <> show oid)) (pure . snd) =<< readObject (envStore env) oid
  let body' = foldl (flip replaceAll) body (coReplace (envOpts env))
  if isBinary body || body' == body
    then pure oid
    else writeObject (envStore env) ObjBlob body'

-- | Git's own heuristic: a NUL byte in the first 8000 bytes means binary.
isBinary :: ByteString -> Bool
isBinary = BS.elem 0 . BS.take 8000

replaceAll :: Replacement -> ByteString -> ByteString
replaceAll (Literal old new) s
  | BS.null old = s
  | otherwise = BS.concat (go s)
  where
    go str = case BS.breakSubstring old str of
      (before, rest)
        | BS.null rest -> [before]
        | otherwise -> before : new : go (BS.drop (BS.length old) rest)
replaceAll (Pattern re new) s = BS.concat (go 0 matches)
  where
    matches = filter ((> 0) . snd) (getAllMatches (match re s))
    go pos [] = [BS.drop pos s]
    go pos ((off, len) : ms) = BS.take (off - pos) (BS.drop pos s) : new : go (off + len) ms

-- Renames ----------------------------------------------------------------------

renamePath :: Store -> Oid -> (ByteString, ByteString) -> IO Oid
renamePath store root (from, to) =
  lookupPath store root (splitPath from) >>= \case
    Nothing -> pure root
    Just entry -> do
      removed <- modifyAt store root (splitPath from) (const Nothing)
      case reverse (splitPath to) of
        name : _ -> modifyAt store removed (splitPath to) (const (Just entry {teName = name}))
        [] -> pure removed

lookupPath :: Store -> Oid -> [ByteString] -> IO (Maybe TreeEntry)
lookupPath store tree = \case
  [] -> pure Nothing
  [name] -> find ((== name) . teName) <$> readTree store tree
  dir : rest ->
    find (\e -> teName e == dir && isTreeMode (teMode e)) <$> readTree store tree >>= \case
      Just e -> lookupPath store (teOid e) rest
      Nothing -> pure Nothing

-- | Replace (or remove, or create) the entry at a path, creating directories
-- on the way down and dropping any that end up empty on the way back up.
modifyAt :: Store -> Oid -> [ByteString] -> (Maybe TreeEntry -> Maybe TreeEntry) -> IO Oid
modifyAt store tree path f = do
  entries <- readTree store tree
  case path of
    [] -> pure tree
    [name] -> write (maybe id (:) (f (find ((== name) . teName) entries)) (without name entries))
    dir : rest -> do
      sub <- case find (\e -> teName e == dir && isTreeMode (teMode e)) entries of
        Just e -> pure (teOid e)
        Nothing -> writeObject store ObjTree ""
      sub' <- modifyAt store sub rest f
      subEntries <- readTree store sub'
      write ([TreeEntry "40000" dir sub' | not (null subEntries)] <> without dir entries)
  where
    without name = filter ((/= name) . teName)
    write = writeObject store ObjTree . renderTree . sortTree

-- | Git sorts entries by name, comparing directories as if they ended in '/'.
sortTree :: Tree -> Tree
sortTree = sortOn (\e -> teName e <> if isTreeMode (teMode e) then "/" else "")

splitPath :: ByteString -> [ByteString]
splitPath = filter (not . BS.null) . BC.split '/'

isTreeMode :: ByteString -> Bool
isTreeMode m = m == "40000" || m == "040000"

isFileMode :: ByteString -> Bool
isFileMode m = m == "100644" || m == "100755" || m == "100664"

-- Globs --------------------------------------------------------------------------

-- | Patterns with a slash match the whole path; without one they match the
-- file or directory name at any depth (like .gitignore). A trailing slash is ignored.
pathMatches :: ByteString -> ByteString -> Bool
pathMatches pat path
  | BC.elem '/' p = globMatch (BC.unpack (BC.dropWhile (== '/') p)) (BC.unpack path)
  | otherwise = globMatch (BC.unpack p) (BC.unpack (last (BC.split '/' path)))
  where
    p = BC.dropWhileEnd (== '/') pat

-- | @*@ and @?@ stay within one path segment; @**@ crosses segments.
globMatch :: String -> String -> Bool
globMatch pat s = case pat of
  '*' : '*' : '/' : ps -> globMatch ps s || or [globMatch ps rest | '/' : rest <- suffixes s]
  '*' : '*' : ps -> any (globMatch ps) (suffixes s)
  '*' : ps -> any (globMatch ps) [drop n s | n <- [0 .. length (takeWhile (/= '/') s)]]
  '?' : ps | c : cs <- s -> c /= '/' && globMatch ps cs
  p : ps | c : cs <- s -> p == c && globMatch ps cs
  [] -> null s
  _ -> False
  where
    suffixes xs = xs : case xs of
      [] -> []
      _ : rest -> suffixes rest

-- Parsing ----------------------------------------------------------------------

-- | Command-line text as UTF-8 bytes, matching how git stores paths and most files.
utf8 :: String -> ByteString
utf8 = BL.toStrict . Builder.toLazyByteString . Builder.stringUtf8

-- | @OLD=>NEW@
parseArrow :: String -> Either String (ByteString, ByteString)
parseArrow s = case BS.breakSubstring "=>" (utf8 s) of
  (a, rest) | not (BS.null rest), not (BS.null a) -> Right (a, BS.drop 2 rest)
  _ -> Left ("expected OLD=>NEW, got " <> show s)

literalReplacement :: String -> Either String Replacement
literalReplacement s = uncurry Literal <$> parseArrow s

-- | The pattern is compiled over UTF-8 bytes, so multibyte characters match
-- literally but can't sit inside a character class.
regexReplacement :: String -> Either String Replacement
regexReplacement s = do
  (pat, new) <- parseArrow s
  case makeRegexM (BC.unpack pat) of
    Just re -> Right (Pattern re new)
    Nothing -> Left ("invalid regex " <> show (BC.unpack pat))
