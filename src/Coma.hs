-- Module Coma contains the "eval" function -- the most important function in
-- the codebase. It receives Coma source code as a String and returns another
-- String -- the output of the script. This way, Main simply passes pure data to
-- us. However, our scipts require file-reading to happen in the process and 
-- that is why we're returning "IO String" instead of just "String".


module Coma (eval, exec) where

import qualified Data.HashMap as HM

import qualified Lexer
import qualified Parser
import qualified Ast



-- EVAL
-- Receive source code of a file and return output. Sometimes, the output is not
-- necessarily CSV, it can be any valid Ast.Coma expression displayed as
-- String.


eval :: String -> IO String
eval code = (exec $ Parser.parse $ Lexer.lex code) >>= (return . show)



-- EXEC
-- Interpreter function. Take Ast.Coma, execute instructions, and return
-- simplified Ast.Coma.


exec :: Ast.Coma -> IO Ast.Coma
exec = execWithEnv (HM.empty)



-- EXEC WITH ENV


execWithEnv :: Ast.Env -> Ast.Coma -> IO Ast.Coma

execWithEnv env int@(Ast.IntAtom _) = return int
execWithEnv env str@(Ast.StrAtom _) = return str

execWithEnv env ident@(Ast.Ident name) = 
  case HM.lookup name env of
    Just coma -> return coma
    Nothing   -> error $ "Unknown identifier: '" ++ name ++ "'"

execWithEnv env lambda@(Ast.Lambda lenv [] expr) = 
  execWithEnv (HM.union env lenv) expr
execWithEnv env lambda@(Ast.Lambda _ _ _) = return lambda

execWithEnv env (Ast.Call (Ast.Lambda lenv (p:params) expr) arg) = do
  arg' <- execWithEnv env arg
  execWithEnv env $ Ast.Lambda (HM.insert p arg' lenv) params expr
execWithEnv env (Ast.Call fn arg) = do
  fn' <- execWithEnv env fn
  execWithEnv env (Ast.Call fn' arg)

execWithEnv _ code = return code
