module Main (main) where

import ContentSpec qualified
import ObjectSpec qualified
import RewriteSpec qualified
import StoreSpec qualified
import Test.Hspec
import TimeSpec qualified
import TuiSpec qualified
import VanitySpec qualified

main :: IO ()
-- Parallel because spawning git on Windows costs ~150ms per call; each test has its own repo.
main = hspec . parallel $ do
  describe "Object" ObjectSpec.spec
  describe "Store" StoreSpec.spec
  describe "Rewrite" RewriteSpec.spec
  describe "Time" TimeSpec.spec
  describe "Content" ContentSpec.spec
  describe "Vanity" VanitySpec.spec
  describe "Tui" TuiSpec.spec
