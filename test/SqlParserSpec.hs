{-# LANGUAGE OverloadedStrings #-}
module SqlParserSpec where

import           Data.Either
import qualified Data.Text                     as T
import           SqlParser
import           Test.Hspec
import           Text.Megaparsec

spec :: Spec
spec = describe "Parsing a subset of the SQL grammar" $ do

    let runParserInTests parser str = runParser parser "tests-input" str

    describe "parsers" $ do

        it "parseSelect" $ do
            runParserInTests parseSelect "Select * from foo"
                `shouldBe` Right (Select [T.pack "*"] [T.pack "foo"])

        it "tables separator" $ do
            runParserInTests tablesSeparator ", " `shouldBe` Right ()

        it "comma" $ do
            runParserInTests comma "," `shouldBe` Right (T.pack ",")
