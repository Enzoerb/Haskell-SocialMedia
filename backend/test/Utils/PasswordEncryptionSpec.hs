module Utils.PasswordEncryptionSpec where

import Test.Hspec
import Utils.PasswordEncryption
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
  context "When I encrypt a password" $ do
    prop "should not be empty" $ do
      encryptPassword "mysecretpassword" `shouldSatisfy` (not . null)

    prop "should be equal for the same password" $ do
      \password -> encryptPassword password `shouldBe` encryptPassword password

  context "When checking a password" $ do
    prop "should be equal to its encrypted version" $ do
      \password -> checkPassword password (encryptPassword password)