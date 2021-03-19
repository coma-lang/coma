module Main where

import System.Environment


main :: IO () 
main = getArgs >>= mode 


mode :: [String] -> IO ()
mode [file] = readFile file >>= putStrLn
mode _      = usage


usage :: IO ()
usage = putStrLn "Usage: coma [help | <file.cql>]"
