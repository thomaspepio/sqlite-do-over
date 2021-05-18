{-# LANGUAGE OverloadedStrings #-}
module SqlParser
    ( Parser
    , Column
    , Table
    , SqlQuery(..)
    , parseSelect
    , tablesSeparator
    , comma
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
parseSelect = undefined

tablesSeparator :: Parser ()
tablesSeparator = comma >> space

comma :: Parser T.Text
comma = string (T.pack ",")
