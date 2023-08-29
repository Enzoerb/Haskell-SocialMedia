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

    it "returns predictable value" $ do
      encryptPassword "as12ma*3-@r" `shouldBe` "4872df144e79a8558f39f771bb1c98a704c0a5719c617249899296ba7c16e0ad"

    it "returns expected value" $ do
      encryptPassword "12345" `shouldBe` "5994471abb01112afcc18159f6cc74b4f511b99806da59b3caf5a9c173cacfc5"