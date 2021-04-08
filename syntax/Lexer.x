{ 
module Lexer where 
}

%wrapper "posn" 

$digit   = 0-9
$graphic = $printable # $white

@string = \" ($graphic # \")*\"
@ident  = [a-zA-Z] [a-zA-Z0-9]*

tokens :-
  $white+           ; 
  "--".*            ; 
  "->"              { \pos _ -> TokenArrow              pos          }
  "!="              { \pos _ -> TokenNotEqual           pos          }
  "="               { \pos _ -> TokenEqual              pos          }
  "<"               { \pos _ -> TokenLessThan           pos          }
  "<="              { \pos _ -> TokenLessThanOrEqual    pos          }
  ">"               { \pos _ -> TokenGreaterThan        pos          }
  ">="              { \pos _ -> TokenGreaterThanOrEqual pos          }
  "++"              { \pos _ -> TokenAppend             pos          }
  "+"               { \pos _ -> TokenAdd                pos          }
  "-"               { \pos _ -> TokenSubtract           pos          }
  "*"               { \pos _ -> TokenMultiply           pos          }
  "/"               { \pos _ -> TokenDivide             pos          }
  "("               { \pos _ -> TokenLeftParen          pos          }
  ")"               { \pos _ -> TokenRightParen         pos          }
  "["               { \pos _ -> TokenLeftBrace          pos          }
  "]"               { \pos _ -> TokenRightBrace         pos          }
  ":="              { \pos _ -> TokenAssign             pos          }
  let               { \pos _ -> TokenLet                pos          }
  in                { \pos _ -> TokenIn                 pos          }
  @ident            { \pos s -> TokenIdentifier         pos s        }
  @string           { \pos s -> TokenString             pos (read s) }
  $digit+           { \pos s -> TokenInteger            pos (read s) } 

{ 
data Token
  = TokenArrow              AlexPosn
  | TokenNotEqual           AlexPosn
  | TokenEqual              AlexPosn
  | TokenLessThan           AlexPosn
  | TokenLessThanOrEqual    AlexPosn
  | TokenGreaterThan        AlexPosn
  | TokenGreaterThanOrEqual AlexPosn
  | TokenAppend             AlexPosn
  | TokenAdd                AlexPosn
  | TokenSubtract           AlexPosn
  | TokenMultiply           AlexPosn
  | TokenDivide             AlexPosn
  | TokenLeftParen          AlexPosn
  | TokenRightParen         AlexPosn
  | TokenLeftBrace          AlexPosn
  | TokenRightBrace         AlexPosn
  | TokenAssign             AlexPosn
  | TokenLet                AlexPosn
  | TokenIn                 AlexPosn
  | TokenIdentifier         AlexPosn String
  | TokenString             AlexPosn String
  | TokenInteger            AlexPosn Int
  deriving (Eq,Show) 

lex = alexScanTokens

tokenPosn :: Token -> String
tokenPosn (TokenArrow              pos  ) = showPosn pos     
tokenPosn (TokenNotEqual           pos  ) = showPosn pos     
tokenPosn (TokenEqual              pos  ) = showPosn pos     
tokenPosn (TokenLessThan           pos  ) = showPosn pos     
tokenPosn (TokenLessThanOrEqual    pos  ) = showPosn pos     
tokenPosn (TokenGreaterThan        pos  ) = showPosn pos     
tokenPosn (TokenGreaterThanOrEqual pos  ) = showPosn pos     
tokenPosn (TokenAppend             pos  ) = showPosn pos     
tokenPosn (TokenAdd                pos  ) = showPosn pos     
tokenPosn (TokenSubtract           pos  ) = showPosn pos     
tokenPosn (TokenMultiply           pos  ) = showPosn pos     
tokenPosn (TokenDivide             pos  ) = showPosn pos     
tokenPosn (TokenLeftParen          pos  ) = showPosn pos     
tokenPosn (TokenRightParen         pos  ) = showPosn pos     
tokenPosn (TokenLeftBrace          pos  ) = showPosn pos
tokenPosn (TokenRightBrace         pos  ) = showPosn pos
tokenPosn (TokenAssign             pos  ) = showPosn pos
tokenPosn (TokenLet                pos  ) = showPosn pos
tokenPosn (TokenIn                 pos  ) = showPosn pos
tokenPosn (TokenIdentifier         pos _) = showPosn pos
tokenPosn (TokenString             pos _) = showPosn pos
tokenPosn (TokenInteger            pos _) = showPosn pos

showPosn :: AlexPosn -> String
showPosn (AlexPn _ x y) = "line " ++ show x ++ ", column " ++ show y
}
