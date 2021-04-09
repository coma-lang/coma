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

%right '->' ':=' in
%nonassoc '=' '!=' '<' '<=' '>' '>='
%left '++'
%left '+' '-'
%left '*' '/'

%% 

Expr        :: { Coma }
Expr        : Expr '='  Expr1          { Equal $1 $3 }
            | Expr '!=' Expr1          { NotEqual $1 $3 }
            | Expr '<'  Expr1          { Less $1 $3 }
            | Expr '<=' Expr1          { LessEqual $1 $3 }
            | Expr '>'  Expr1          { Greater $1 $3 }
            | Expr '>=' Expr1          { GreaterEqual $1 $3 }
            | Expr '++' Expr1          { Append $1 $3 }
            | Expr '+'  Expr1          { Add $1 $3 }
            | Expr '-'  Expr1          { Subtract $1 $3 }
            | Expr '*'  Expr1          { Multiply $1 $3 }
            | Expr '/'  Expr1          { Divide $1 $3 }
            | Expr1                    { $1 }

Expr1       :: { Coma }
Expr1       : Expr1 Literal            { Call $1 $2 }
            | Literal                  { $1 }

Literal     :: { Coma }
Literal     : integer                  { IntAtom $1 }
            | string                   { StrAtom $1}
            | ident                    { Ident $1 }
            | '(' Expr ')'             { $2 }
            | '[' List ']'             { List $2 }
            | '(' Params '->' Expr ')' { Lambda (reverse $2) $4 }

List        :: { [Coma] }
List        : {- empty -}              { [] }
            | Literal List             { $1 : $2 }

Params      :: { [String] }
Params      : ident                    { [$1] }
            | Params '->' ident        { $3 : $1 }

{ 
parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 

type Env = HM.Map String Coma

data Coma
  = IntAtom Int
  | StrAtom String
  | Ident String
  | List [Coma]
  | Lambda [String] Coma
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
  deriving Show
}
