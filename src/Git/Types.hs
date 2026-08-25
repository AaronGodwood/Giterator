module Git.Types
  ( Oid
  , oidFromRaw
  , oidToRaw
  , oidFromHex
  , oidToHex
  , shortHex
  , ObjectType (..)
  , objectTypeName
  , parseObjectType
  , Signature (..)
  , Commit (..)
  , TreeEntry (..)
  , Tree
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BC
import Data.Int (Int64)

-- | 20 raw SHA-1 bytes: the form trees store, and cheaper to hash and compare.
newtype Oid = Oid ByteString
  deriving (Eq, Ord)

instance Show Oid where
  show = BC.unpack . oidToHex

oidFromRaw :: ByteString -> Maybe Oid
oidFromRaw bs
  | BS.length bs == 20 = Just (Oid bs)
  | otherwise = Nothing

oidToRaw :: Oid -> ByteString
oidToRaw (Oid bs) = bs

-- | Lowercase only: git always writes lowercase, and accepting uppercase would
-- break byte-exact round trips of commit headers.
oidFromHex :: ByteString -> Maybe Oid
oidFromHex bs
  | BS.length bs == 40 && BC.all (`elem` ("0123456789abcdef" :: String)) bs =
      either (const Nothing) oidFromRaw (Base16.decode bs)
  | otherwise = Nothing

oidToHex :: Oid -> ByteString
oidToHex (Oid bs) = Base16.encode bs

shortHex :: Oid -> ByteString
shortHex = BS.take 7 . oidToHex

data ObjectType = ObjBlob | ObjTree | ObjCommit | ObjTag
  deriving (Eq, Show)

objectTypeName :: ObjectType -> ByteString
objectTypeName = \case
  ObjBlob   -> "blob"
  ObjTree   -> "tree"
  ObjCommit -> "commit"
  ObjTag    -> "tag"

parseObjectType :: ByteString -> Maybe ObjectType
parseObjectType = \case
  "blob"   -> Just ObjBlob
  "tree"   -> Just ObjTree
  "commit" -> Just ObjCommit
  "tag"    -> Just ObjTag
  _        -> Nothing

data Signature = Signature
  { sigName  :: ByteString
  , sigEmail :: ByteString
  , sigTime  :: Int64
  , sigTz    :: ByteString
    -- ^ Kept as the original text (e.g. "+0100") so odd offsets in old repos round-trip.
  }
  deriving (Eq, Show)

data Commit = Commit
  { cTree      :: Oid
  , cParents   :: [Oid]
  , cAuthor    :: Signature
  , cCommitter :: Signature
  , cExtra     :: [(ByteString, ByteString)]
    -- ^ gpgsig, encoding, mergetag, ... in original order; multi-line values joined with '\n'.
  , cMessage   :: ByteString
  }
  deriving (Eq, Show)

data TreeEntry = TreeEntry
  { teMode :: ByteString
    -- ^ Text, not a number: some old repos wrote "040000", which must survive a rewrite.
  , teName :: ByteString
  , teOid  :: Oid
  }
  deriving (Eq, Show)

type Tree = [TreeEntry]
