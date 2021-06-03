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
    ) where

import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Column = T.Text
type Table = T.Text

data SqlQuery = Select [Column] [Table] -- SELECT col1, col2 FROM table1, table2
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
columns = many column

column :: Parser Column
column = do
    col <- many alphaNumChar
    commaSeparator <|> space
    return $ T.pack col

tables :: Parser [Table]
tables = undefined

commaSeparator :: Parser ()
commaSeparator = comma >> space

comma :: Parser T.Text
comma = string (T.pack ",")
