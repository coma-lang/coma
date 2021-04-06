module Wasif (p2, p3) where 

import Data.List
import Data.Maybe

import qualified Csv
import qualified Core
 


-- PROBLEM 2


p2 :: Csv.Table -> Csv.Table
p2 = sort . mapMaybe p2forEach


p2forEach :: Csv.Row -> Maybe Csv.Row
p2forEach row = 
  if Core.get [0] row == Core.get [1] row
    then Just $ Core.get [2,0] row
    else Nothing



-- PROBLEM 3


p3 :: Csv.Table -> Csv.Table -> Csv.Table
p3 p q = sort $ mapMaybe (uncurry p3forEach) (Core.join p q)

p3forEach :: Csv.Row -> Csv.Row -> Maybe Csv.Row
p3forEach pr qr = 
  if Core.get [0] pr == Core.get [0] qr
    then Just $ Core.get [0] pr 
      ++ Core.merge (Core.get [1..3] pr) (Core.get [1..3] qr)
    else Nothing
