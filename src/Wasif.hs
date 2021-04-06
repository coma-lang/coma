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
p3forEach pr qr = if p1 == Core.value qr 0
                    then Just [p1, Core.safeGet qr pr 1, Core.safeGet qr pr 2, Core.safeGet qr pr 3]
                    else Nothing
          where p1 = Core.value pr 0

{--
p3forEach :: Csv.Row -> Csv.Row -> Maybe Csv.Row
p3forEach (p1:pr) (q1:qr) =
  if p1 == q1 then Just $ p1 : map (uncurry p3r) (zip pr qr) else Nothing
--}

p3r :: String -> String -> String
p3r p q = if null p then q else p
