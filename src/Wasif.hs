module Wasif (p2, p3) where 

import Data.List
import Data.Maybe

import qualified Csv
 


-- PROBLEM 2


p2 :: Csv.Table -> Csv.Table
p2 = sort . mapMaybe p2forEach


p2forEach :: Csv.Row -> Maybe Csv.Row
p2forEach [a1,a2,a3] = if a1 == a2 then Just [a3,a1] else Nothing



-- PROBLEM 3


p3 :: Csv.Table -> Csv.Table -> Csv.Table
p3 p q = sort $ mapMaybe (uncurry p3forEach) (cartesianProduct p q)


p3forEach :: Csv.Row -> Csv.Row -> Maybe Csv.Row
p3forEach (p1:pr) (q1:qr) =
  if p1 == q1 then Just $ p1 : map (uncurry p3r) (zip pr qr) else Nothing


p3r :: String -> String -> String
p3r p q = if null p then q else p


cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
