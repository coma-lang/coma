-- DO NOT ABUSE THIS MODULE!
-- I created this in order to collect here some tests that don't work as 
-- expected in our standard testing environment. The coreJoin test, for example,
-- would pass no matter the changes in the expected structure...


module MainTest where

import qualified Ast
import qualified Core
import Debug.Trace (trace)



-- MAIN


test :: IO ()
test = do
  join <- coreJoin
  print join



-- CORE.JOIN


coreJoin :: IO Ast.Coma
coreJoin = do
  let a = Ast.List
            [ Ast.List [Ast.StrAtom "1", Ast.StrAtom "2"]
            , Ast.List [Ast.StrAtom "5", Ast.StrAtom "6"]
            ]

  let b = Ast.List
            [ Ast.List [Ast.StrAtom "3", Ast.StrAtom "4"]
            , Ast.List [Ast.StrAtom "7", Ast.StrAtom "8"]
            ]
        
  {- EXPECTED OUTPUT
    [ ["1","2","3","4"]
    , ["1","2","7","8"]
    , ["5","6","7","8"]
    , ["5","6","7","8"]
    ]
  -}

  Core.doubleJoin a b
