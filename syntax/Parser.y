{ 
module Ast where 
import qualified Data.HashMap as HM
import Data.List (intercalate)
import Lexer 
}

%name parse 
%tokentype { Token } 
%error { parseError }
%token 
  '('     { TokenLeftParen          pos }
  ')'     { TokenRightParen         pos }
  '['     { TokenLeftBrace          pos }
  ']'     { TokenRightBrace         pos }

  '!='    { TokenNotEqual           pos }
  '='     { TokenEqual              pos }
  '<'     { TokenLessThan           pos }
  '<='    { TokenLessThanOrEqual    pos }
  '>'     { TokenGreaterThan        pos }
  '>='    { TokenGreaterThanOrEqual pos }

  '++'    { TokenAppend             pos }

  '+'     { TokenAdd                pos }
  '-'     { TokenSubtract           pos }
  '*'     { TokenMultiply           pos }
  '/'     { TokenDivide             pos }

  '\\'    { TokenLambda             pos }
  '->'    { TokenArrow              pos }

  ':='    { TokenAssign             pos }
  let     { TokenLet                pos }
  in      { TokenIn                 pos }

  ident   { TokenIdentifier         pos $$ }
  string  { TokenString             pos $$ }
  integer { TokenInteger            pos $$ } 

%right '->' ':=' in
%nonassoc '=' '!=' '<' '<=' '>' '>='
%left '++'
%left '+' '-'
%left '*' '/'

%% 

Let         :: { Coma }
Let         : let ident ':=' Expr in Let { Let $2 $4 $6 }
            | Expr                       { $1 }

Expr        :: { Coma }
Expr        : Expr '='  Call             { Equal $1 $3 }
            | Expr '!=' Call             { NotEqual $1 $3 }
            | Expr '<'  Call             { Less $1 $3 }
            | Expr '<=' Call             { LessEqual $1 $3 }
            | Expr '>'  Call             { Greater $1 $3 }
            | Expr '>=' Call             { GreaterEqual $1 $3 }
            | Expr '++' Call             { Append $1 $3 }
            | Expr '+'  Call             { Add $1 $3 }
            | Expr '-'  Call             { Subtract $1 $3 }
            | Expr '*'  Call             { Multiply $1 $3 }
            | Expr '/'  Call             { Divide $1 $3 }
            | Call                       { $1 }

Call        :: { Coma }
Call        : Call Literal               { Call $1 $2 }
            | Literal                    { $1 }

Literal     :: { Coma }
Literal     : integer                    { IntAtom $1 }
            | string                     { StrAtom $1}
            | ident                      { Ident $1 }
            | '(' Expr ')'               { $2 }
            | '[' List ']'               { List $2 }
            | '\\' ident '->' Literal    { Lambda 0 HM.empty (lambda $2 $4) }

List        :: { [Coma] }
List        : {- empty -}                { [] }
            | Literal List               { $1 : $2 }

{ 
parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 



-- ENV


type Env = HM.Map String Coma



-- COMA


data Coma
  = IntAtom Int
  | StrAtom String
  | Ident String
  | List [Coma]
  | Lambda Int Env (Int -> Env -> Coma -> IO Coma)
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
  


-- EQ


instance Eq Coma where
  IntAtom i == IntAtom j = i == j
  StrAtom i == StrAtom j = i == j
  Ident   i == Ident   j = i == j
  List    i == List    j = j == j
  _         == _         = False



-- SHOW COMA


instance Show Coma where
  show (IntAtom i) = "#" ++ show i
  show (StrAtom s) = s
  show (Ident idt) = "$" ++ idt
  show (List list) = "[ " ++ unwords (map show list) ++ " ]"
  show (Lambda i env _) = "<lambda/" ++ show i ++ "/" ++ show env ++ ">"
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
  show (Call e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Let idt e1 e2) = "let " ++ idt ++ " := " ++ show e1 ++ " in " ++ show e2



-- EXEC WITH ENV


execWithEnv :: Env -> Coma -> IO Coma

execWithEnv _ int@(IntAtom _) = return int
execWithEnv _ str@(StrAtom _) = return str
execWithEnv _ lst@(List    _) = return lst

execWithEnv env ident@(Ident name) = 
  case HM.lookup name env of
    Just coma -> return coma
    Nothing   -> error $ "Unknown identifier: '" ++ name ++ "'"

execWithEnv env (Lambda i lenv fn) = 
  return $ Lambda i (HM.union lenv env) fn
    
execWithEnv env (Call e1 e2) = do
  Lambda i lenv fn <- execWithEnv env e1
  arg <- execWithEnv env e2
  fn i (HM.union env lenv) arg

execWithEnv env (Let ident expr inexpr) = do
  evaluated <- execWithEnv env expr
  execWithEnv (HM.insert ident evaluated env) inexpr 
 
execWithEnv _ code = return code



-- LAMBDA


lambda :: String -> Ast.Coma -> Int -> Ast.Env -> Ast.Coma -> IO Ast.Coma
lambda param expr _ env arg = execWithEnv (HM.insert param arg env) expr
}
