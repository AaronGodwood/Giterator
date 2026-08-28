module Main (main) where

import ObjectSpec qualified
import RewriteSpec qualified
import StoreSpec qualified
import Test.Hspec

main :: IO ()
-- Parallel because spawning git on Windows costs ~150ms per call; each test has its own repo.
main = hspec . parallel $ do
  describe "Object" ObjectSpec.spec
  describe "Store" StoreSpec.spec
  describe "Rewrite" RewriteSpec.spec
