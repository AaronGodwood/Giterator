-- | Reading objects through one long-running @git cat-file --batch@ process
-- (rather than spawning git per object). New objects are buffered in memory
-- and written out together as a single packfile.
module Git.Store
  ( Store
  , storeRepo
  , withStore
  , lookupObject
  , readObject
  , readCommit
  , readTree
  , writeObject
  , flushObjects
  , revList
  , runGit
  , runGitInput
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Git.Object (hashObject, parseCommit, parseTree)
import Git.Pack (buildPack)
import Git.Types
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering)
import System.Process.Typed

data Store = Store
  { storeRepo    :: FilePath
  , storeIn      :: Handle
  , storeOut     :: Handle
  , storePending :: IORef Pending
  }

-- | Objects written but not yet flushed, and their total size in bytes.
data Pending = Pending !(Map Oid (ObjectType, ByteString)) !Int

-- | Bounds memory when rewriting large files; most rewrites flush once, at the end.
flushThreshold :: Int
flushThreshold = 64 * 1024 * 1024

-- | Pending objects are flushed when the action returns normally. If it throws
-- they are discarded, which is safe: no ref can point at them yet.
withStore :: FilePath -> (Store -> IO a) -> IO a
withStore repo act = do
  pending <- newIORef (Pending Map.empty 0)
  withProcessWait_ config $ \p -> do
    let hin = getStdin p
        hout = getStdout p
    -- Binary mode matters on Windows: text mode would translate line endings in object bodies.
    hSetBinaryMode hin True
    hSetBinaryMode hout True
    hSetBuffering hin (BlockBuffering Nothing)
    let store = Store repo hin hout pending
    result <- act store
    flushObjects store
    -- cat-file only exits once its stdin closes, and withProcessWait_ waits for that exit.
    result <$ hClose hin
  where
    config =
      setStdin createPipe . setStdout createPipe . setWorkingDir repo $
        proc "git" ["cat-file", "--batch"]

writeObject :: Store -> ObjectType -> ByteString -> IO Oid
writeObject s t body = do
  let oid = hashObject t body
  Pending objects size <- readIORef (storePending s)
  unless (Map.member oid objects) $ do
    let size' = size + BS.length body
    writeIORef (storePending s) (Pending (Map.insert oid (t, body) objects) size')
    when (size' > flushThreshold) (flushObjects s)
  pure oid

-- | Write every pending object as one pack. Must happen before any ref points
-- at them: @update-ref@ refuses to point a ref at a missing object.
flushObjects :: Store -> IO ()
flushObjects s = do
  objects <- atomicModifyIORef' (storePending s) (\(Pending m _) -> (Pending Map.empty 0, Map.elems m))
  unless (null objects) $
    runGitInput s ["index-pack", "--stdin"] (buildPack objects)

-- | Look up any name git understands (@HEAD@, @main~2@, @HEAD:README.md@, a hash).
-- Only sees flushed objects; 'readObject' also sees pending ones.
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
readObject s oid = do
  Pending objects _ <- readIORef (storePending s)
  case Map.lookup oid objects of
    Just found -> pure (Just found)
    Nothing -> fmap (\(_, t, body) -> (t, body)) <$> lookupObject s (oidToHex oid)

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

-- | Output is discarded (index-pack prints the pack name); errors still reach stderr.
runGitInput :: Store -> [String] -> ByteString -> IO ()
runGitInput s args input =
  runProcess_
    . setStdin (byteStringInput (BL.fromStrict input))
    . setStdout nullStream
    . setWorkingDir (storeRepo s)
    $ proc "git" args
