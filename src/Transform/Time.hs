-- | Timestamp transforms. Each is a pure function on one signature; 'onDates'
-- chooses whether it applies to author dates, committer dates or both.
module Transform.Time
  ( Which (..)
  , TzMode (..)
  , TimeOptions (..)
  , defaultTimeOptions
  , timePlan
  , onDates
  , shiftBy
  , rescale
  , jitter
  , workHours
  , setTz
  , parseDuration
  , parseRange
  , parseWindow
  , parseTz
  ) where

import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Git.Date (tzOffsetMinutes)
import Git.Rewrite (Plan, pureTransform, withOriginal)
import Git.Types

data Which = Author | Committer | Both
  deriving (Eq, Show)

data TzMode = KeepInstant | KeepWallClock
  deriving (Eq, Show)

data TimeOptions = TimeOptions
  { toWhich  :: Which
  , toSpread :: Maybe (Int64, Int64)
  , toShift  :: Maybe Int64
  , toJitter :: Maybe Int64
  , toSeed   :: ByteString
  , toWindow :: Maybe (Int64, Int64)
  , toTz     :: Maybe (ByteString, TzMode)
  }
  deriving (Show)

defaultTimeOptions :: TimeOptions
defaultTimeOptions = TimeOptions Both Nothing Nothing Nothing "giterator" Nothing Nothing

-- | Applied in a fixed order: spread, shift, jitter, work hours, timezone.
-- Work hours comes after jitter so jittered times still land inside the window.
timePlan :: TimeOptions -> Plan
timePlan o = maybe mempty spread (toSpread o) <> const (mconcat steps)
  where
    w = toWhich o
    spread target history = case concatMap (datesOf w . snd) history of
      [] -> mempty
      ts -> pureTransform (onDates w (rescale (minimum ts, maximum ts) target))
    steps =
      catMaybes
        [ pureTransform . onDates w . shiftBy <$> toShift o
        , (\j -> withOriginal (onDates w . jitter (toSeed o) j)) <$> toJitter o
        , pureTransform . onDates w . workHours <$> toWindow o
        , (\(tz, mode) -> pureTransform (onDates w (setTz mode tz))) <$> toTz o
        ]

onDates :: Which -> (Signature -> Signature) -> Commit -> Commit
onDates w f c =
  c
    { cAuthor = if w /= Committer then f (cAuthor c) else cAuthor c
    , cCommitter = if w /= Author then f (cCommitter c) else cCommitter c
    }

datesOf :: Which -> Commit -> [Int64]
datesOf w c = map sigTime ([cAuthor c | w /= Committer] <> [cCommitter c | w /= Author])

shiftBy :: Int64 -> Signature -> Signature
shiftBy d s = s {sigTime = sigTime s + d}

-- | Linearly map @[lo, hi]@ onto @[from, to]@: order and relative gaps survive,
-- so bursts of activity still look like bursts.
rescale :: (Int64, Int64) -> (Int64, Int64) -> Signature -> Signature
rescale (lo, hi) (from, to) s = s {sigTime = fromInteger t'}
  where
    t'
      | hi == lo = toInteger from
      | otherwise =
          toInteger from
            + (toInteger (sigTime s) - toInteger lo) * (toInteger to - toInteger from)
              `div` (toInteger hi - toInteger lo)

-- | A pseudo-random offset in @[-range, range]@ derived from the seed and the
-- commit's original id: reproducible, and author/committer move together.
jitter :: ByteString -> Int64 -> Oid -> Signature -> Signature
jitter seed range oid s
  | range <= 0 = s
  | otherwise = s {sigTime = sigTime s + offset}
  where
    digest = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) (0 :: Word64) (BS.take 8 (SHA1.hash (seed <> oidToRaw oid)))
    offset = fromIntegral (digest `mod` fromIntegral (2 * range + 1)) - range

-- | Squeeze each local day linearly into @[start, end)@ (seconds since local
-- midnight). Every commit is mapped, even ones already inside the window: a map
-- that left those alone could put a 08:00 commit after a 10:00 one.
workHours :: (Int64, Int64) -> Signature -> Signature
workHours (start, end) s = s {sigTime = day * 86400 + start + sod * (end - start) `div` 86400 - off}
  where
    off = 60 * fromIntegral (fromMaybe 0 (tzOffsetMinutes (sigTz s)))
    (day, sod) = (sigTime s + off) `divMod` 86400

-- | 'KeepInstant' only relabels the offset (the wall clock changes);
-- 'KeepWallClock' keeps the local time and moves the instant.
setTz :: TzMode -> ByteString -> Signature -> Signature
setTz mode tz s = s {sigTz = tz, sigTime = time}
  where
    minutes = fromIntegral . fromMaybe 0 . tzOffsetMinutes
    time = case mode of
      KeepInstant -> sigTime s
      KeepWallClock -> sigTime s + 60 * (minutes (sigTz s) - minutes tz)

-- Parsers for the CLI --------------------------------------------------------

-- | @3d@, @2h30m@, @-1w@, @+90s@.
parseDuration :: String -> Either String Int64
parseDuration input = case input of
  '-' : rest -> negate <$> units rest
  '+' : rest -> units rest
  _ -> units input
  where
    bad = Left ("bad duration " <> show input <> " (examples: 3d, 2h30m, -1w)")
    units "" = bad
    units s = go s
    go "" = Right 0
    go s = case span isDigit s of
      (ds@(_ : _), u : rest) | Just k <- lookup u table -> (read ds * k +) <$> go rest
      _ -> bad
    table = [('s', 1), ('m', 60), ('h', 3600), ('d', 86400), ('w', 604800)]

-- | @2024-01-01..2024-03-01@ or with times, @2024-01-01T09:00..2024-01-02T18:00@ (UTC).
parseRange :: String -> Either String (Int64, Int64)
parseRange input = case breakOn ".." input of
  Just (a, b) -> do
    from <- parseDateTime a
    to <- parseDateTime b
    if from < to then Right (from, to) else Left "range must go forwards in time"
  Nothing -> Left ("bad range " <> show input <> " (example: 2024-01-01..2024-03-01)")

parseDateTime :: String -> Either String Int64
parseDateTime s = case [t | f <- ["%Y-%m-%d", "%Y-%m-%dT%H:%M", "%Y-%m-%dT%H:%M:%S"], Just t <- [parseTimeM False defaultTimeLocale f s]] of
  (t : _) -> Right (floor (utcTimeToPOSIXSeconds (t :: UTCTime)))
  [] -> Left ("bad date " <> show s <> " (examples: 2024-01-31, 2024-01-31T09:30)")

-- | @09:00-18:00@ as seconds since midnight.
parseWindow :: String -> Either String (Int64, Int64)
parseWindow input = case break (== '-') input of
  (a, '-' : b) -> do
    start <- clock a
    end <- clock b
    if start < end then Right (start, end) else Left "window must start before it ends"
  _ -> Left ("bad window " <> show input <> " (example: 09:00-18:00)")
  where
    clock [h1, h2, ':', m1, m2]
      | all isDigit [h1, h2, m1, m2]
      , let h = read [h1, h2]
      , let m = read [m1, m2]
      , h <= 24 && m < 60 && h * 60 + m <= 1440 =
          Right ((h * 60 + m) * 60)
    clock s = Left ("bad time " <> show s <> " (expected HH:MM)")

-- | @+0900@, @-0530@.
parseTz :: String -> Either String ByteString
parseTz s = case tzOffsetMinutes (BC.pack s) of
  Just _ -> Right (BC.pack s)
  Nothing -> Left ("bad timezone " <> show s <> " (examples: +0100, -0500)")

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go ""
  where
    go _ [] = Nothing
    go acc rest@(c : cs)
      | needle `isPrefixOf` rest = Just (reverse acc, drop (length needle) rest)
      | otherwise = go (c : acc) cs
