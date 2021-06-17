module Storage where

import qualified Data.Map as M
import qualified Data.Text as T

type ColumnName = String
type Row = M.Map ColumnName T.Text
type Table = [Row]

