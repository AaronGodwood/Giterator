{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators for values git could actually have written, so that
-- @parse . render == Just@ is a fair property to demand.
module Gen () where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromJust)
import Git.Types
import Test.QuickCheck

bytesWithout :: [Char] -> Gen ByteString
bytesWithout banned = BS.pack <$> listOf (arbitrary `suchThat` (`notElem` map (toEnum . fromEnum) banned))

nonEmptyBytesWithout :: [Char] -> Gen ByteString
nonEmptyBytesWithout banned = bytesWithout banned `suchThat` (not . BS.null)

instance Arbitrary Oid where
  arbitrary = fromJust . oidFromRaw . BS.pack <$> vectorOf 20 arbitrary

instance Arbitrary Signature where
  arbitrary =
    Signature
      <$> bytesWithout "<>\n"
      <*> bytesWithout "<>\n"
      <*> arbitrary
      <*> genTz
    where
      genTz = do
        sign <- elements "+-"
        digits <- vectorOf 4 (elements ['0' .. '9'])
        pure (BC.pack (sign : digits))

instance Arbitrary Commit where
  arbitrary =
    Commit
      <$> arbitrary
      <*> resize 3 (listOf arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> resize 3 (listOf ((,) <$> nonEmptyBytesWithout " \n" <*> arbitrary))
      <*> arbitrary

instance Arbitrary TreeEntry where
  arbitrary =
    TreeEntry
      <$> elements ["100644", "100755", "40000", "120000", "160000", "040000"]
      <*> nonEmptyBytesWithout "\0"
      <*> arbitrary

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary
