{-# LANGUAGE OverloadedStrings #-}
module SqlParser
    ( Parser
    , Column
    , Table
    , SqlQuery(..)
    , Value(..)
    , parseSelect
    , parseInsert
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
              | Insert Table [Value T.Text]
    deriving (Show, Eq)

data Value a = StringV a
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

parseInsert :: Parser SqlQuery
parseInsert = do
    string "INSERT INTO"
    space
    singleTable <- table
    space
    string "VALUES("
    vals <- values
    string ")"
    return $ Insert singleTable vals

columns :: Parser [Column]
columns = manyTextSepByComma column

column :: Parser Column
column = manyText

tables :: Parser [Table]
tables = manyTextSepByComma table

table :: Parser Table
table = manyText

singleValue :: Parser (Value T.Text)
singleValue = do 
    char '\'' 
    chars <- manyTill asciiChar (char '\'')
    let text = T.pack chars
    return $ StringV text


values :: Parser [Value T.Text]
values = singleValue `sepBy` commaSeparator

manyText :: Parser T.Text
manyText = T.pack <$> many alphaNumChar

manyTextSepByComma :: Parser T.Text -> Parser [T.Text]
manyTextSepByComma parser = parser `sepBy` commaSeparator

commaSeparator :: Parser ()
commaSeparator = comma >> space

comma :: Parser T.Text
comma = string (T.pack ",")
