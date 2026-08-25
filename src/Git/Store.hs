-- | Reading objects from a repository through one long-running
-- @git cat-file --batch@ process, rather than spawning git per object.
module Git.Store
  ( Store
  , storeRepo
  , withStore
  , lookupObject
  , readObject
  , readCommit
  , readTree
  , revList
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Git.Object (parseCommit, parseTree)
import Git.Types
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering)
import System.Process.Typed

data Store = Store
  { storeRepo :: FilePath
  , storeIn   :: Handle
  , storeOut  :: Handle
  }

withStore :: FilePath -> (Store -> IO a) -> IO a
withStore repo act = withProcessWait_ config $ \p -> do
  let hin = getStdin p
      hout = getStdout p
  -- Binary mode matters on Windows: text mode would translate line endings in object bodies.
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  hSetBuffering hin (BlockBuffering Nothing)
  -- cat-file only exits once its stdin closes, and withProcessWait_ waits for that exit.
  act (Store repo hin hout) <* hClose hin
  where
    config =
      setStdin createPipe . setStdout createPipe . setWorkingDir repo $
        proc "git" ["cat-file", "--batch"]

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
revList s args = do
  out <- readProcessStdout_ (setWorkingDir (storeRepo s) (proc "git" ("rev-list" : args)))
  traverse parseLine (BC.lines (BL.toStrict out))
  where
    parseLine l = maybe (fail ("unexpected rev-list output: " <> BC.unpack l)) pure (oidFromHex l)
