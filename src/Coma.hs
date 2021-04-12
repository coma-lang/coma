-- Module Coma contains the "eval" function -- the most important function in
-- the codebase. It receives Coma source code as a String and returns another
-- String -- the output of the script. This way, Main simply passes pure data to
-- us. However, our scipts require file-reading to happen in the process and 
-- that is why we're returning "IO String" instead of just "String".


module Coma (eval, exec) where

import qualified Data.HashMap as HM

import qualified Lexer
import qualified Ast
import qualified Core



-- EVAL
-- Receive source code of a file and return output. Sometimes, the output is not
-- necessarily CSV, it can be any valid Ast.Coma expression displayed as
-- String.


eval :: String -> IO String
eval code = (exec $ Ast.parse $ Lexer.lex code) >>= (return . show)



-- EXEC
-- Interpreter function. Take Ast.Coma, execute instructions, and return
-- simplified Ast.Coma.


exec :: Ast.Coma -> IO Ast.Coma
exec = Ast.execWithEnv env



-- ENV
-- Contains Coma's standard library of functions.


env :: HM.Map String Ast.Coma
env = HM.fromList
  [ ("read", Ast.Lambda HM.empty Core.read)
  , ("join", Ast.Lambda HM.empty Core.join)
  ]
