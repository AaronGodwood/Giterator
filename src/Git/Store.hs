-- | Reading objects through one long-running @git cat-file --batch@ process
-- (rather than spawning git per object), and writing loose objects directly.
module Git.Store
  ( Store
  , storeRepo
  , withStore
  , lookupObject
  , readObject
  , readCommit
  , readTree
  , writeObject
  , revList
  , runGit
  , runGitInput
  ) where

import Codec.Compression.Zlib qualified as Zlib
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Git.Object (hashObject, objectHeader, parseCommit, parseTree)
import Git.Types
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering, openBinaryTempFile)
import System.Process.Typed

data Store = Store
  { storeRepo    :: FilePath
  , storeObjects :: FilePath
  , storeIn      :: Handle
  , storeOut     :: Handle
  }

withStore :: FilePath -> (Store -> IO a) -> IO a
withStore repo act = do
  -- --git-path rather than assuming .git/objects: handles bare repos and GIT_OBJECT_DIRECTORY.
  objects <- BC.unpack . trimNewline . BL.toStrict
    <$> readProcessStdout_ (setWorkingDir repo (proc "git" ["rev-parse", "--git-path", "objects"]))
  withProcessWait_ (config repo) $ \p -> do
    let hin = getStdin p
        hout = getStdout p
    -- Binary mode matters on Windows: text mode would translate line endings in object bodies.
    hSetBinaryMode hin True
    hSetBinaryMode hout True
    hSetBuffering hin (BlockBuffering Nothing)
    -- cat-file only exits once its stdin closes, and withProcessWait_ waits for that exit.
    act (Store repo (repo </> objects) hin hout) <* hClose hin
  where
    config r =
      setStdin createPipe . setStdout createPipe . setWorkingDir r $
        proc "git" ["cat-file", "--batch"]

-- | Store an object as a loose file: zlib of header <> body, at objects/ab/cdef...
-- Written to a temp file then renamed, so a crash never leaves a truncated object.
writeObject :: Store -> ObjectType -> ByteString -> IO Oid
writeObject s t body = do
  let oid = hashObject t body
      (dirName, fileName) = BS.splitAt 2 (oidToHex oid)
      dir = storeObjects s </> BC.unpack dirName
      target = dir </> BC.unpack fileName
  exists <- doesFileExist target
  unless exists $ do
    createDirectoryIfMissing False dir
    (tmp, h) <- openBinaryTempFile dir "tmp_obj_"
    BL.hPut h (Zlib.compress (BL.fromStrict (objectHeader t body <> body)))
    hClose h
    renameFile tmp target
  pure oid

-- | Look up any name git understands (@HEAD@, @main~2@, @HEAD:README.md@, a hash).
lookupObject :: Store -> ByteString -> IO (Maybe (Oid, ObjectType, ByteString))
lookupObject s name
  | BC.elem '\n' name = pure Nothing
  | otherwise = do
      BS.hPut (storeIn s) (name <> "\n")
      hFlush (storeIn s)
      header <- BS.hGetLine (storeOut s)
      case BC.words header of
        [hex, ty, size]
          | Just oid <- oidFromHex hex
          , Just t <- parseObjectType ty
          , Just (n, "") <- BC.readInt size -> do
              body <- BS.hGet (storeOut s) n
              _ <- BS.hGet (storeOut s) 1
              pure (Just (oid, t, body))
        _ -> pure Nothing

readObject :: Store -> Oid -> IO (Maybe (ObjectType, ByteString))
readObject s oid = fmap (\(_, t, body) -> (t, body)) <$> lookupObject s (oidToHex oid)

readCommit :: Store -> Oid -> IO Commit
readCommit s oid = readObject s oid >>= \case
  Just (ObjCommit, body) | Just c <- parseCommit body -> pure c
  _ -> fail ("not a readable commit: " <> show oid)

readTree :: Store -> Oid -> IO Tree
readTree s oid = readObject s oid >>= \case
  Just (ObjTree, body) | Just t <- parseTree body -> pure t
  _ -> fail ("not a readable tree: " <> show oid)

revList :: Store -> [String] -> IO [Oid]
revList s args = runGit s ("rev-list" : args) >>= traverse parseLine . BC.lines
  where
    parseLine l = maybe (fail ("unexpected rev-list output: " <> BC.unpack l)) pure (oidFromHex l)

runGit :: Store -> [String] -> IO ByteString
runGit s args = BL.toStrict <$> readProcessStdout_ (setWorkingDir (storeRepo s) (proc "git" args))

runGitInput :: Store -> [String] -> ByteString -> IO ()
runGitInput s args input =
  runProcess_ . setStdin (byteStringInput (BL.fromStrict input)) . setWorkingDir (storeRepo s) $ proc "git" args

trimNewline :: ByteString -> ByteString
trimNewline = BC.dropWhileEnd (`elem` ("\r\n" :: String))
