module LibSpec where

import Test.Hspec

spec :: Spec
spec = describe "Lib spec" $ do
    it "fails" $ do
        1 + 1 `shouldBe` 3