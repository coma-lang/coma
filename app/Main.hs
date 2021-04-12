module Main where

import qualified System.Environment as Env

import qualified Repl
import qualified Coma
import qualified MainTest



-- MAIN


main :: IO () 
main = Env.getArgs >>= mode 



-- MODE


mode :: [String] -> IO ()
mode ["repl"] = Repl.loop
mode ["test"] = MainTest.test
mode [file]   = readFile file >>= Coma.eval >>= putStrLn
mode _        = usage



-- USAGE


usage :: IO ()
usage = putStrLn "Usage: coma [<file.cql> | repl | help]"
