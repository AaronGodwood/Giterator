-- | Building packfiles. Writing thousands of loose objects costs ~7ms of
-- filesystem overhead each on Windows; one pack handed to @git index-pack@
-- costs a single file and a single process.
module Git.Pack
  ( buildPack
  ) where

import Codec.Compression.Zlib qualified as Zlib
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, lazyByteString, toLazyByteString, word32BE, word8)
import Data.ByteString.Lazy qualified as BL
import Git.Types

-- | A version 2 pack of whole (undeltified) objects: @PACK@, version, count,
-- the entries, then a SHA-1 of everything before it.
buildPack :: [(ObjectType, ByteString)] -> ByteString
buildPack objects = body <> SHA1.hash body
  where
    body =
      BL.toStrict . toLazyByteString $
        byteString "PACK" <> word32BE 2 <> word32BE (fromIntegral (length objects)) <> foldMap entry objects
    entry (t, content) =
      entryHeader (typeCode t) (BS.length content) <> lazyByteString (Zlib.compress (BL.fromStrict content))

typeCode :: ObjectType -> Int
typeCode = \case
  ObjCommit -> 1
  ObjTree   -> 2
  ObjBlob   -> 3
  ObjTag    -> 4

-- | First byte: continuation bit, 3-bit type, low 4 bits of the size; then the
-- rest of the size 7 bits at a time, least significant first.
entryHeader :: Int -> Int -> Builder
entryHeader code size = word8 (fromIntegral first) <> rest (size `shiftR` 4)
  where
    first = continue (size `shiftR` 4) .|. (code `shiftL` 4) .|. (size .&. 0x0f)
    rest n
      | n == 0 = mempty
      | otherwise = word8 (fromIntegral (continue (n `shiftR` 7) .|. (n .&. 0x7f))) <> rest (n `shiftR` 7)
    continue n = if n > 0 then 0x80 else 0
