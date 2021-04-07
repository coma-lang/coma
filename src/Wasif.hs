module Wasif (p2, p3) where 

import Data.List
import Data.Maybe

import qualified Csv
import qualified Core
 


-- PROBLEM 2


p2 :: Csv.Table -> Csv.Table
p2 
  = sort 
  . Core.select [2,0] 
  . Core.given (\row -> Core.get [0] row == Core.get [1] row)



-- PROBLEM 3


p3 :: Csv.Table -> Csv.Table -> Csv.Table
p3 p q 
  = sort 
  $ map p3forEach
  $ Core.given (\row -> Core.get [0] row == Core.get [4] row) 
  $ Core.join p q


p3forEach :: Csv.Row -> Csv.Row
p3forEach row = 
  --      row : p1 p2 p3 p4   q1 q2 q3 q4
  --      ids : 0  1  2  3    4  5  6  7
  Core.get [0] row 
  ++ Core.merge (Core.get [1..3] row) (Core.get [5..7] row)
