module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Service.IdenticonService as IdenticonService

main :: IO ()
main = hspec $ do
  describe "Identicon" $ do
    it "is not null" $ do
      IdenticonService.generateIdenticon "1" >>= (`shouldSatisfy` (not . null))

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException