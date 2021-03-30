module Csv
  ( Row
  , Table
  , parse
  , serialize
  ) where

import Data.List (intercalate)
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



-- SERIALIZE


showRow :: Csv.Row -> String
showRow = intercalate coma


serialize :: Csv.Table -> String
serialize = unlines . map showRow
