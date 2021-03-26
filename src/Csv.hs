module Csv where

import Data.List.Split (splitOn)



-- TYPES


type Row = [String]
type Table = [Row]



-- PARSE


parse :: String -> Table
parse = map (splitOn ",") . words

