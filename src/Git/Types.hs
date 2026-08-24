module Git.Types
  ( Oid
  , oidFromHex
  , oidToHex
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC

-- Stored as hex for now; switches to 20 raw bytes once we parse tree entries ourselves.
newtype Oid = Oid ByteString
  deriving (Eq, Ord)

instance Show Oid where
  show = BC.unpack . oidToHex

oidFromHex :: ByteString -> Maybe Oid
oidFromHex bs
  | BS.length bs == 40 && BC.all isHex bs = Just (Oid bs)
  | otherwise = Nothing
  where
    isHex c = c `elem` ("0123456789abcdef" :: String)

oidToHex :: Oid -> ByteString
oidToHex (Oid bs) = bs
