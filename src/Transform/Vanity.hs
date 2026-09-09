-- | Vanity hashes: nudge a commit's bytes until its id starts with a chosen
-- hex prefix. Each extra hex digit multiplies the expected work by 16.
--
-- A search is a constant prefix, hashed once, plus a suffix that varies with a
-- counter; each attempt only hashes the suffix, continuing from the saved
-- SHA-1 state of the prefix.
module Transform.Vanity
  ( Method (..)
  , VanityOptions (..)
  , Target
  , targetHex
  , parseTarget
  , matchesTarget
  , Search (..)
  , whitespaceSearch
  , secondsSearch
  , mine
  , vanityPlan
  ) where

import Control.Concurrent (forkIO, getNumCapabilities, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Bits (shiftR, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BC
import Data.Char (digitToInt, isHexDigit, toLower)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Word (Word8)
import Git.Object (authorTimeOffset, hashObject, renderCommit, stripSignature)
import Git.Rewrite (Plan, Transform (..))
import Git.Types

data Method = Whitespace | Seconds
  deriving (Eq, Show)

data VanityOptions = VanityOptions
  { voTarget :: Target
  , voMethod :: Method
  , voChain  :: Bool
    -- ^ Mine every rewritten commit rather than only branch tips.
  }

-- | The prefix as typed (lowercased), the whole bytes to match, and for odd
-- lengths the high nibble of the next byte.
data Target = Target ByteString ByteString (Maybe Word8)
  deriving (Show)

targetHex :: Target -> ByteString
targetHex (Target hex _ _) = hex

parseTarget :: String -> Either String Target
parseTarget input
  | null s || length s > 16 = Left "prefix must be 1 to 16 hex digits"
  | not (all isHexDigit s) = Left ("not hex: " <> show input)
  | otherwise = case Base16.decode (BC.pack whole) of
      Right bytes -> Right (Target (BC.pack s) bytes (fromIntegral . digitToInt <$> nibble))
      Left err -> Left err
  where
    s = map toLower input
    whole = take (2 * (length s `div` 2)) s
    nibble = if odd (length s) then Just (last s) else Nothing

matchesTarget :: Target -> ByteString -> Bool
matchesTarget (Target _ bytes nibble) digest =
  bytes `BS.isPrefixOf` digest
    && maybe True (\n -> BS.index digest (BS.length bytes) `shiftR` 4 == n) nibble

data Search = Search
  { sContext :: SHA1.Ctx
    -- ^ State after hashing the object header and the constant part of the body.
  , sSuffix  :: Int -> Maybe ByteString
    -- ^ The rest of the body for attempt @n@; Nothing skips it.
  , sLimit   :: Int
  , sBuild   :: Int -> Commit
  }

-- | Spaces and tabs spelling out @n@ in binary, at the end of the message's
-- last line (before its newline): git keeps trailing whitespace and nobody
-- sees it. Existing trailing whitespace is replaced, so mining again doesn't
-- pile up padding. Room for 4096x the expected attempts.
whitespaceSearch :: Int -> Commit -> Search
whitespaceSearch digits c = Search context (Just . suffix) (2 ^ width) build
  where
    width = min 48 (4 * digits + 12)
    (text, newline) = case BS.stripSuffix "\n" (cMessage c) of
      Just t -> (t, "\n")
      Nothing -> (cMessage c, "")
    trimmed = c {cMessage = BC.dropWhileEnd (`elem` (" \t" :: String)) text}
    padding n = fst (BS.unfoldrN width (\i -> Just (if testBit n i then 9 else 32, i + 1)) (0 :: Int))
    suffix n = padding n <> newline
    body = renderCommit trimmed
    context = SHA1.update SHA1.init (header (BS.length body + width + BS.length newline) <> body)
    build n = trimmed {cMessage = cMessage trimmed <> suffix n}

-- | Move the author and committer times later by up to a window sized for 16x
-- the expected attempts (capped at a day). Attempts that change the number of
-- digits in a timestamp would change the object size, so they're skipped.
secondsSearch :: Int -> Commit -> Search
secondsSearch digits c = Search context suffix (side * side) shifted
  where
    window = min 86400 (ceiling (sqrt (16 * 16 ^^ digits :: Double)))
    side = window + 1
    shifted n =
      let (dc, da) = n `divMod` side
       in c
            { cAuthor = (cAuthor c) {sigTime = sigTime (cAuthor c) + fromIntegral da}
            , cCommitter = (cCommitter c) {sigTime = sigTime (cCommitter c) + fromIntegral dc}
            }
    body = renderCommit c
    offset = authorTimeOffset c
    context = SHA1.update SHA1.init (header (BS.length body) <> BS.take offset body)
    suffix n =
      let b = renderCommit (shifted n)
       in if BS.length b == BS.length body then Just (BS.drop offset b) else Nothing

header :: Int -> ByteString
header size = "commit " <> BC.pack (show size) <> "\0"

-- | The lowest matching attempt, searched in parallel chunks across all
-- capabilities. Taking the lowest (not the first found) keeps results
-- reproducible however the threads are scheduled.
mine :: Target -> Search -> IO (Maybe Int)
mine target s = getNumCapabilities >>= go 0
  where
    chunk = 65536
    go start caps
      | start >= sLimit s = pure Nothing
      | otherwise = do
          let starts = take caps (takeWhile (< sLimit s) [start, start + chunk ..])
          vars <- forM starts $ \from -> do
            var <- newEmptyMVar
            _ <- forkIO (putMVar var $! searchChunk from (min (sLimit s) (from + chunk)))
            pure var
          results <- traverse takeMVar vars
          case catMaybes results of
            n : _ -> pure (Just n)
            [] -> go (start + caps * chunk) caps
    searchChunk from to = find hit [from .. to - 1]
    hit n = maybe False (matchesTarget target . SHA1.finalize . SHA1.update (sContext s)) (sSuffix s n)

-- | Signatures are stripped before mining: the engine would strip them from
-- any changed commit anyway, and doing it afterwards would change the hash.
-- The callback gets the original id, the new id and the number of attempts.
vanityPlan :: VanityOptions -> (Oid -> Oid -> Int -> IO ()) -> Plan
vanityPlan o onMined history = pure . Transform $ \oid c ->
  if voChain o || oid `Set.member` tips
    then liftIO (mineCommit oid (stripSignature c))
    else pure c
  where
    parents = Set.fromList (concatMap (cParents . snd) history)
    tips = Set.fromList [oid | (oid, _) <- history, not (oid `Set.member` parents)]
    target = voTarget o
    digits = BS.length (targetHex target)
    idOf = hashObject ObjCommit . renderCommit
    mineCommit oid c
      | matchesTarget target (oidToRaw (idOf c)) = c <$ onMined oid (idOf c) 0
      | otherwise = do
          let search = (if voMethod o == Whitespace then whitespaceSearch else secondsSearch) digits c
          mine target search >>= \case
            Just n -> do
              let mined = sBuild search n
              onMined oid (idOf mined) (n + 1)
              pure mined
            Nothing ->
              fail $
                "no id starting with " <> BC.unpack (targetHex target) <> " found for " <> show oid
                  <> " within the search space; try --method whitespace or a shorter prefix"
