module Ast where

import qualified Data.HashMap as HM
import Data.List (intercalate)



-- ENV


type Env = HM.Map String Coma



-- COMA


data Coma
  = IntAtom Int
  | StrAtom String
  | Ident String
  | List [Coma]
  | Lambda Env [String] Coma
  | Equal Coma Coma
  | NotEqual Coma Coma
  | Less Coma Coma
  | LessEqual Coma Coma
  | Greater Coma Coma
  | GreaterEqual Coma Coma
  | Append Coma Coma
  | Add Coma Coma
  | Subtract Coma Coma
  | Multiply Coma Coma
  | Divide Coma Coma
  | Call Coma Coma
  | Let String Coma Coma
  deriving Eq
  


-- SHOW COMA


instance Show Coma where
  show (IntAtom i) = show i
  show (StrAtom s) = show s
  show (Ident idt) = idt
  show (List list) = "[ " ++ unwords (map show list) ++ " ]"
  show (Lambda _ params expr) = 
    "( " ++ intercalate " -> " params ++ " -> " ++ show expr ++ " )"
  show (Equal e1 e2) = show e1 ++ " = " ++ show e2
  show (NotEqual e1 e2) = show e1 ++ " != " ++ show e2
  show (Less e1 e2) = show e1 ++ " < " ++ show e2
  show (LessEqual e1 e2) = show e1 ++ " <= " ++ show e2
  show (Greater e1 e2) = show e1 ++ " > " ++ show e2
  show (GreaterEqual e1 e2) = show e1 ++ " >= " ++ show e2
  show (Append e1 e2) = show e1 ++ " ++ " ++ show e2
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Subtract e1 e2) = show e1 ++ " - " ++ show e2
  show (Multiply e1 e2) = show e1 ++ " * " ++ show e2
  show (Divide e1 e2) = show e1 ++ " / " ++ show e2
  show (Call e1 e2) = show e1 ++ " " ++ show e2
  show (Let idt e1 e2) = "let " ++ idt ++ " := " ++ show e1 ++ " in " ++ show e2
