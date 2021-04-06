module Tim (p4, p5) where 

import Data.List
import Data.Maybe

import qualified Csv
import qualified Core
 


-- PROBLEM 4


p4 :: Csv.Table -> Csv.Table
p4 = sort . Core.given (not . Core.empty . Core.value 1)



-- PROBLEM 5


p5 :: Csv.Table -> Csv.Table
p5 = sort . map p5forEach


p5forEach :: Csv.Row -> Csv.Row
p5forEach row = first ++ ["0"] ++ first
  where first = Core.get [ 0 ] row
