module Testify where



-- RESULT


data Result
  = Pass
  | Fail String
  deriving Show



-- TEST


test :: String -> Result -> IO ()
test name result = putStrLn $ name ++ ": " ++ show result



-- EQ


eq :: Eq a => a -> a -> Result
eq x y 
  | x == y    = Pass
  | otherwise = Fail "equality check failed"
