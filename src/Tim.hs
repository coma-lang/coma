module Tim (p4, p5) where 

import Data.List
import Data.Maybe

import qualified Csv
import qualified Core
 


-- PROBLEM 4


p4 :: Csv.Table -> Csv.Table
p4 [] = []
p4 (t@(a1:a2:[]):rs) | a2=="" = sort(p4 rs)
                     | otherwise = sort(t:(p4 rs))
p4 _ = error "incorrect input"



-- PROBLEM 5


p5 :: Csv.Table -> Csv.Table
p5 [] = []
p5 ((a1:[]):rs) = sort((a1:"0":a1:[]):(p5 rs))
p5 _ = error "incorrect input"
