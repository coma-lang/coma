module Core 
  ( join
  , get
  , select
  , pairUp
  , value
  , firstOr
  , merge
  , given
  , empty
  ) where

import Data.Maybe

import qualified Csv



-- JOIN (CARTESIAN PRODUCT)
-- Combine 2 CSV tables by creating a list of Csv.Row pairs. The length of the
-- output list is X * Y where X and Y are respective length of the input tables.


join :: Csv.Table -> Csv.Table -> Csv.Table
join xs ys = map (uncurry (++)) $ [(x,y) | x <- xs, y <- ys]



-- PAIR UP
-- Combine 2 CSV tables by creating a list of Csv.Row pairs. Unlike join, this
-- function does not operate as a forEach loop, but rather follows the behaviour
-- of the Prelude.zip function.


pairUp :: Csv.Table -> Csv.Table -> Csv.Table
pairUp a b = map (uncurry (++)) $ Prelude.zip a b



-- GET
-- Create a Csv.Row with just the elements under selected indices.


get :: [Int] -> Csv.Row -> Csv.Row
get indices row = map (row !!) indices



-- SELECT
-- Create a Csv.Table with just the columns under selected indices.


select :: [Int] -> Csv.Table -> Csv.Table
select indices = map (get indices)



-- VALUE
-- Look at the value at given index in row.


value :: Int -> Csv.Row -> String
value index row = row !! index



-- FIRST OR


firstOr :: String -> String -> String
firstOr x y = if empty x then y else x



-- MERGE
-- Merge two rows, giving priority to the first one. Use values from the second
-- row if the value in the first row is an empty string.


merge :: Csv.Row -> Csv.Row -> Csv.Row
merge p q = map (uncurry firstOr) $ Prelude.zip p q



-- GIVEN
-- Filter out table rows that don't fit the predicate.


given :: (Csv.Row -> Bool) -> Csv.Table -> Csv.Table
given predicate table = mapMaybe selector table
  where selector row = if predicate row then Just row else Nothing



-- EMPTY
-- Checks if CSV value is an empty string.


empty :: String -> Bool
empty = null