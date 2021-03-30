module Viktor (p1) where

import qualified Csv
import qualified Core



-- PROBLEM 1


p1 :: Csv.Table -> Csv.Table -> Csv.Table
p1 a b = map (uncurry (++)) (Core.cartesianProduct a b)
