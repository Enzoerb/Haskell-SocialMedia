module Service.IdenticonServiceSpec where

import Test.Hspec
import qualified Service.IdenticonService as IdenticonService
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad

spec :: Spec
spec = do
  context "When I generate a Identicon" $ do
    prop "is not empty" $
      IdenticonService.generateIdenticon
       >=> (`shouldSatisfy` (not . null))

    prop "is equal for the same hash" $
      \hash -> IdenticonService.generateIdenticon hash >>= \value0 ->
        IdenticonService.generateIdenticon hash >>= \value1 ->
          quickCheck (value0 == value1)

    it "is different for different hashes" $
      pendingWith "How to guarantee different hashes in the test"