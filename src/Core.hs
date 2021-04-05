module Core 
  ( join
  , get
  , select
  , Core.zip
  ) where

import qualified Csv



-- JOIN (CARTESIAN PRODUCT)
-- Combine 2 CSV tables by creating a list of Csv.Row pairs. The length of the
-- output list is X * Y where X and Y are respective length of the input tables.


join :: Csv.Table -> Csv.Table -> [(Csv.Row, Csv.Row)]
join xs ys = [(x,y) | x <- xs, y <- ys]



-- ZIP
-- Combine 2 CSV tables by creating a list of Csv.Row pairs. Unlike join, this
-- function does not operate as a forEach loop, but rather follows the behaviour
-- of the Prelude.zip function.


zip :: Csv.Table -> Csv.Table -> [(Csv.Row, Csv.Row)]
zip = Prelude.zip



-- GET
-- Create a Csv.Row with just the elements under selected indices.


get :: [Int] -> Csv.Row -> Csv.Row
get indices row = map (row !!) indices



-- SELECT
-- Create a Csv.Table with just the columns under selected indices.


select :: [Int] -> Csv.Table -> Csv.Table
select indices = map (get indices)
