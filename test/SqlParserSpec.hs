{-# LANGUAGE OverloadedStrings #-}
module SqlParserSpec where

import           Data.Either
import qualified Data.Text                     as T
import           SqlParser
import           Test.Hspec
import           Text.Megaparsec

spec :: Spec
spec = describe "Parsing a subset of the SQL grammar" $ do

    it "fails" $ do
        1 + 1 `shouldBe` 3

    it "parses" $ do
        runParser parseSelect "Select * from foo" (T.pack "tests")
            `shouldBe` Right (Select [T.pack "*"] [T.pack "foo"])
