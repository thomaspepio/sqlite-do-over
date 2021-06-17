module StorageSpec where

import qualified Data.Text                     as T
import           Test.Hspec

spec :: Spec
spec = describe "Storage must store" $ do
    it "should store" $ do
        1 + 1 `shouldBe` 3