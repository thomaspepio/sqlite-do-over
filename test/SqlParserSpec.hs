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

        describe "select" $ do
            it "parseSelect" $ do
                runParserInTests parseSelect "SELECT * FROM foo"
                    `shouldBe` Right (Select [T.pack "*"] [T.pack "foo"])

        describe "reusable" $ do
            it "column" $ do
                runParserInTests column "col1 " `shouldBe` Right (T.pack "col1")
                -- runParserInTests column "col1, "
                --     `shouldBe` Right (T.pack "col1")

            it "columns" $ do
                runParserInTests columns "col1, col2, col3"
                    `shouldBe` Right (T.pack <$> ["col1", "col2", "col3"])

            it "comma separator" $ do
                runParserInTests commaSeparator ", " `shouldBe` Right ()

            it "comma" $ do
                runParserInTests comma "," `shouldBe` Right (T.pack ",")

