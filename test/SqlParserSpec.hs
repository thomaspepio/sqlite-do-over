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
            it "parseSelect with named columns and tables" $ do
                runParserInTests parseSelect "SELECT foo, bar FROM baz, qux"
                    `shouldBe` Right
                                   (Select (T.pack <$> ["foo", "bar"])
                                           (T.pack <$> ["baz", "qux"])
                                   )

            it "parseSelect with wildcard columns and named table" $ do -- peut être qu'on ne veut pas
                runParserInTests parseSelect "SELECT * FROM foo"        -- supporter la wildcart tout de suite ?
                    `shouldBe` Right (Select [T.pack "*"] [T.pack "foo"])

        describe "reusable" $ do
            it "column" $ do
                runParserInTests column "col1" `shouldBe` Right (T.pack "col1")

            it "columns" $ do
                runParserInTests columns "col1, col2, col3"
                    `shouldBe` Right (T.pack <$> ["col1", "col2", "col3"])

            it "table" $ do
                runParserInTests table "tab1" `shouldBe` Right (T.pack "tab1")

            it "tables" $ do
                runParserInTests tables "tab1, tab2, tab3"
                    `shouldBe` Right (T.pack <$> ["tab1", "tab2", "tab3"])

            it "comma separator" $ do
                runParserInTests commaSeparator ", " `shouldBe` Right ()

            it "comma" $ do
                runParserInTests comma "," `shouldBe` Right (T.pack ",")

