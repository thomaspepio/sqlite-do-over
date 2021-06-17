{-# LANGUAGE OverloadedStrings #-}
module SqlParser
    ( Parser
    , Column
    , Table
    , SqlQuery(..)
    , parseSelect
    , columns
    , column
    , tables
    , commaSeparator
    , comma
    , table
    ) where

import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Column = T.Text
type Table = T.Text

data SqlQuery = Select [Column] [Table]
    deriving (Show, Eq)

type Parser = Parsec Void T.Text

parseSelect :: Parser SqlQuery
parseSelect = do
    string "SELECT"
    space
    cols <- columns
    space
    string "FROM"
    space
    tabs <- tables
    return $ Select cols tabs

columns :: Parser [Column]
columns = manyTextSepByComma column

column :: Parser Column
column = manyText

tables :: Parser [Table]
tables = manyTextSepByComma table

table :: Parser Table
table = manyText

manyText :: Parser T.Text
manyText = T.pack <$> many alphaNumChar

manyTextSepByComma :: Parser T.Text -> Parser [T.Text]
manyTextSepByComma parser = parser `sepBy` commaSeparator

commaSeparator :: Parser ()
commaSeparator = comma >> space

comma :: Parser T.Text
comma = string (T.pack ",")
