module Csv
  ( Row
  , Table
  , parse
  , coma
  ) where

import Data.List.Split (splitOn)
import Data.List.Extra (trim)



-- TYPES


type Row = [String]
type Table = [Row]



-- CONST


coma :: String
coma = ","



-- PARSE


parse :: String -> Table
parse = map (map trim . splitOn coma) . lines
