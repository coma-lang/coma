-- Module Coma contains the "exec" function -- the most important function in
-- the codebase. It receives Coma source code as a String and returns another
-- String -- the CSV output of the script. This way, Main simply passes pure
-- data to us. However, our scipts require file-reading to happen in the process
-- and that is why we're returning "IO String" instead of just "String".


module Coma (exec) where

import qualified Lexer



-- EXEC
-- Receive source code of a file and return CSV output.


exec :: String -> IO String
exec = return . show . Lexer.tokenize
