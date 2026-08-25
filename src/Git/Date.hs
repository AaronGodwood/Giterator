module Git.Date
  ( tzOffsetMinutes
  , formatSignatureDate
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime, minutesToTimeZone, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Git.Types (Signature (..))

-- | @"+0130"@ -> 90, @"-0500"@ -> -300.
tzOffsetMinutes :: ByteString -> Maybe Int
tzOffsetMinutes tz = case BC.unpack tz of
  [sign, h1, h2, m1, m2]
    | sign `elem` ("+-" :: String) && all isDigit [h1, h2, m1, m2] ->
        let minutes = read [h1, h2] * 60 + read [m1, m2]
         in Just (if sign == '-' then negate minutes else minutes)
  _ -> Nothing

-- | The wall-clock time the committer saw, followed by their offset. Malformed
-- offsets fall back to UTC so odd historical commits still display.
formatSignatureDate :: Signature -> ByteString
formatSignatureDate s = BC.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" local) <> " " <> sigTz s
  where
    zone = minutesToTimeZone (fromMaybe 0 (tzOffsetMinutes (sigTz s)))
    local = utcToLocalTime zone (posixSecondsToUTCTime (fromIntegral (sigTime s)))
