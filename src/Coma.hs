-- Module Coma contains the "eval" function -- the most important function in
-- the codebase. It receives Coma source code as a String and returns another
-- String -- the CSV output of the script. This way, Main simply passes pure
-- data to us. However, our scipts require file-reading to happen in the process
-- and that is why we're returning "IO String" instead of just "String".


module Coma (eval) where

import qualified Lexer
import qualified Parser



-- EVAL
-- Receive source code of a file and return CSV output.


eval :: String -> IO String
eval = return . show . Parser.parse . Lexer.lex
