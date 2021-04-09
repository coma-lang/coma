-- Module Coma contains the "eval" function -- the most important function in
-- the codebase. It receives Coma source code as a String and returns another
-- String -- the output of the script. This way, Main simply passes pure data to
-- us. However, our scipts require file-reading to happen in the process and 
-- that is why we're returning "IO String" instead of just "String".


module Coma (eval) where

import qualified Lexer
import qualified Parser



-- EVAL
-- Receive source code of a file and return output. Sometimes, the output is not
-- necessarily CSV, it can be any valid Parser.Coma expression displayed as
-- String.


eval :: String -> IO String
eval code = (exec $ Parser.parse $ Lexer.lex code) >>= (return . show)



-- EXEC
-- Interpreter function. Take Parser.Coma (AST), execute instructions, and 
-- return simplified Parser.Coma.


exec :: Parser.Coma -> IO Parser.Coma
exec = return