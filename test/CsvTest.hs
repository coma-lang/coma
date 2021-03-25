module CsvTest where

import qualified Testify
import qualified Csv



-- MAIN


main :: IO ()
main = Testify.test "parse" $ Testify.eq expect $ Csv.parse given



-- GIVEN


given :: String
given =
  " 1,2,3 \
  \ 4,5,6 \
  \ 7,8,9 "



-- EXPECT


expect :: Csv.Table
expect = [["1","2","3"],["4","5","6"],["7","8","9"]]

