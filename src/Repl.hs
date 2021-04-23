module Repl (loop) where

import Control.Monad (forever)
import qualified System.IO as Buf

import qualified Coma



-- READ


read :: IO String
read = aux "" 0
  where
    aux string 1 = return string
    aux string i = do
      printFlush "❮❮❮ "
      line <- Buf.getLine
      aux (string ++ line ++ "\n") (fromEnum (null line) + i)



-- PRINT


print :: String -> IO ()
print output = do
  mapM_ (putStrLn . ("❯❯❯ " ++)) (lines output)
  putStr "\n"



-- PRINT AND FLUSH


printFlush :: String -> IO ()
printFlush string = do
  putStr string
  Buf.hFlush Buf.stdout



-- LOOP
-- Spin up an infinite loop that reads from stdin, evaluates, and prints the
-- evaluated expression to stdout.


loop :: IO ()
loop = forever $ Repl.read >>= Coma.eval >>= Repl.print
