module Csv where



-- TYPES


type Row = [String]
type Table = [Row]



-- PARSE


parse :: String -> Table
parse = map (words . commasAreSpaces) . words
    where commasAreSpaces = map (\c -> if c == ',' then ' ' else c)

