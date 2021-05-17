module SqlParser
    ( Parser
    , Column
    , Table
    , SqlQuery(..)
    , parseSelect
    ) where

import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec

type Column = T.Text
type Table = T.Text

data SqlQuery = Select [Column] [Table]
    deriving (Show, Eq)

type Parser = Parsec Void T.Text

parseSelect :: Parser SqlQuery
parseSelect = undefined
