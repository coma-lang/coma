module Main where

import qualified System.Environment as Env

import qualified Csv



-- MAIN


main :: IO () 
main = Env.getArgs >>= mode 



-- MODE


mode :: [String] -> IO ()
mode [file] = readFile file >>= (print . Csv.parse)
mode _      = usage



-- USAGE


usage :: IO ()
usage = putStrLn "Usage: coma [help | <file.cql>]"

