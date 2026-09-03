module TimeSpec (spec) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Gen ()
import Git.Date (tzOffsetMinutes)
import Git.Types
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Transform.Time

-- Keeps arithmetic well away from Int64 overflow: roughly ±35000 years.
realisticTime :: Gen Int64
realisticTime = choose (-(2 ^ (40 :: Int)), 2 ^ (40 :: Int))

genTz :: Gen ByteString
genTz = elements ["+0000", "+0100", "-0500", "+0530", "+1400", "-1200", "+0945"]

sigAt :: Int64 -> ByteString -> Signature
sigAt = Signature "A" "a@example.com"

localSecondOfDay :: Signature -> Int64
localSecondOfDay s = (sigTime s + 60 * fromIntegral (fromMaybe 0 (tzOffsetMinutes (sigTz s)))) `mod` 86400

genWindow :: Gen (Int64, Int64)
genWindow = do
  a <- choose (0, 86399)
  b <- choose (a + 1, 86400)
  pure (a, b)

spec :: Spec
spec = do
  describe "onDates" $
    prop "only touches the chosen dates" $ \c ->
      let c' = onDates Author (shiftBy 10) c
       in cCommitter c' === cCommitter c .&&. sigTime (cAuthor c') === sigTime (cAuthor c) + 10

  describe "rescale" $ do
    prop "maps the ends of the source range onto the target" $
      forAll ((,) <$> realisticTime <*> realisticTime) $ \(lo, hi) ->
        lo < hi ==>
          sigTime (rescale (lo, hi) (0, 1000) (sigAt lo "+0000")) === 0
            .&&. sigTime (rescale (lo, hi) (0, 1000) (sigAt hi "+0000")) === 1000
    prop "keeps commits in order" $
      forAll (sort <$> vectorOf 3 realisticTime) $ \ts -> case ts of
        [lo, mid, hi] ->
          let f t = sigTime (rescale (lo, hi) (100, 200) (sigAt t "+0000"))
           in f lo <= f mid && f mid <= f hi
        _ -> False

  describe "jitter" $ do
    prop "stays within the range" $ \oid (Positive range) ->
      forAll realisticTime $ \t ->
        abs (sigTime (jitter "seed" range oid (sigAt t "+0000")) - t) <= range
    prop "moves author and committer by the same amount" $ \oid ->
      let s = sigAt 1700000000 "+0000"
          c = onDates Both (jitter "seed" 3600 oid) (Commit oid [] s s [] "")
       in cAuthor c === cCommitter c
    it "depends on the seed" $
      length (nub [sigTime (jitter seed 100000 exampleOid (sigAt 1700000000 "+0000")) | seed <- ["a", "b", "c", "d"]])
        `shouldSatisfy` (> 1)

  describe "workHours" $ do
    prop "lands every commit inside the window, in local time" $
      forAll ((,,) <$> genWindow <*> realisticTime <*> genTz) $ \((a, b), t, tz) ->
        let sod = localSecondOfDay (workHours (a, b) (sigAt t tz))
         in sod >= a .&&. sod < b
    prop "keeps commits in order" $
      forAll ((,,,) <$> genWindow <*> realisticTime <*> realisticTime <*> genTz) $ \(w, t1, t2, tz) ->
        let f t = sigTime (workHours w (sigAt t tz))
         in f (min t1 t2) <= f (max t1 t2)

  describe "setTz" $ do
    prop "KeepInstant keeps the moment" $
      forAll ((,,) <$> realisticTime <*> genTz <*> genTz) $ \(t, from, to) ->
        sigTime (setTz KeepInstant to (sigAt t from)) === t
    prop "KeepWallClock keeps the local clock time" $
      forAll ((,,) <$> realisticTime <*> genTz <*> genTz) $ \(t, from, to) ->
        let s' = setTz KeepWallClock to (sigAt t from)
         in localSecondOfDay s' === localSecondOfDay (sigAt t from) .&&. sigTz s' === to

  describe "parsers" $ do
    it "read durations" $ do
      parseDuration "3d" `shouldBe` Right 259200
      parseDuration "-2h30m" `shouldBe` Right (-9000)
      parseDuration "+1w" `shouldBe` Right 604800
      parseDuration "90" `shouldSatisfy` isLeft
      parseDuration "-" `shouldSatisfy` isLeft
    it "read ranges" $ do
      parseRange "2024-01-01..2024-03-01" `shouldBe` Right (1704067200, 1709251200)
      parseRange "2024-01-01T12:00..2024-01-02" `shouldBe` Right (1704110400, 1704153600)
      parseRange "2024-03-01..2024-01-01" `shouldSatisfy` isLeft
    it "read windows and timezones" $ do
      parseWindow "09:00-18:00" `shouldBe` Right (32400, 64800)
      parseWindow "18:00-09:00" `shouldSatisfy` isLeft
      parseTz "+0900" `shouldBe` Right "+0900"
      parseTz "UTC" `shouldSatisfy` isLeft
  where
    isLeft = either (const True) (const False)

exampleOid :: Oid
exampleOid = fromMaybe (error "bad oid") (oidFromHex "c81fffe02461796e9159e3705ed2045a22145e38")
