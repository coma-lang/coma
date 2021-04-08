module Repl (loop) where

import Control.Monad (forever)
import qualified System.IO as Buf
import Data.Foldable

import qualified Coma



-- READ
-- Print out some helpful prefix to express that we're expecting some input.
-- Then read one line from stdin and return it.


read :: IO String
read = do
    Repl.printFlush "λ <- "
    Buf.getLine



-- PRINT


print :: String -> IO ()
print string = putStrLn $ "λ -> " ++ string ++ "\n"



-- EVAL


eval :: IO ()
eval = Repl.read >>= Coma.exec >>= Repl.print



-- LOOP
-- Spin up an infinite loop that reads from stdin, evaluates, and prints the
-- evaluated expression to stdout.


loop :: IO ()
loop = forever $ Repl.eval



-- UTILITY FUNCTIONS


printFlush :: String -> IO ()
printFlush string = do
    Prelude.putStr string
    Buf.hFlush Buf.stdout
