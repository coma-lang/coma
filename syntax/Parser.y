{ 
module Parser where 
import qualified Data.HashMap as HM
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

  '->'    { TokenArrow              pos }

  ':='    { TokenAssign             pos }
  let     { TokenLet                pos }
  in      { TokenIn                 pos }

  ident   { TokenIdentifier         pos $$ }
  string  { TokenString             pos $$ }
  integer { TokenInteger            pos $$ } 

%nonassoc '=' '!=' '<' '<=' '>' '>='
%left '++'
%left '+' '-'
%left '*' '/'
%right '->' ':=' in

%% 

Expr        :: { Coma }
            : Expr '='  Expr      { Equal $1 $3 }
            | Expr '!=' Expr      { NotEqual $1 $3 }
            | Expr '<'  Expr      { Less $1 $3 }
            | Expr '<=' Expr      { LessEqual $1 $3 }
            | Expr '>'  Expr      { Greater $1 $3 }
            | Expr '>=' Expr      { GreaterEqual $1 $3 }
            | Expr '++' Expr      { Append $1 $3 }
            | Expr '+'  Expr      { Add $1 $3 }
            | Expr '-'  Expr      { Subtract $1 $3 }
            | Expr '*'  Expr      { Multiply $1 $3 }
            | Expr '/'  Expr      { Divide $1 $3 }
            | Literal             { $1 }

Literal     :: { Coma }
Literal     : integer             { IntAtom $1 }
            | string              { StrAtom $1}
            | ident               { Ident $1 }
            | '(' Expr ')'        { $2 }

{ 
parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 

type Env = HM.Map String Coma

data Coma
  = IntAtom Int
  | StrAtom String
  | Ident String
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
  deriving Show
}
