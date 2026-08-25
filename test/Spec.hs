module Main (main) where

import ObjectSpec qualified
import StoreSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Object" ObjectSpec.spec
  describe "Store" StoreSpec.spec
