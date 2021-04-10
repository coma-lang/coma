module CoreTest where

import Test.Tasty.Hspec

import qualified Core
import qualified Ast



-- READ


coreRead :: Spec
coreRead = do

  numbers <- runIO $ Core.read $ Ast.StrAtom "test/data/numbers.csv"

  it "read" $
    numbers `shouldBe` 
    Ast.List 
      [ Ast.List [Ast.StrAtom "1", Ast.StrAtom "2", Ast.StrAtom "3"]
      , Ast.List [Ast.StrAtom "4", Ast.StrAtom "5", Ast.StrAtom "6"]
      , Ast.List [Ast.StrAtom "7", Ast.StrAtom "8", Ast.StrAtom "9"]
      ]



-- JOIN


coreJoin :: Spec
coreJoin = do

  it "join" $
    Core.join
      ( Ast.List 
        [ Ast.List [Ast.StrAtom "1", Ast.StrAtom "2"]
        , Ast.List [Ast.StrAtom "5", Ast.StrAtom "6"]
        ]
      )
      ( Ast.List 
        [ Ast.List [Ast.StrAtom "3", Ast.StrAtom "4"]
        , Ast.List [Ast.StrAtom "7", Ast.StrAtom "8"]
        ]
      ) `shouldBe`
      Ast.List 
        [ Ast.List 
          [Ast.StrAtom "1", Ast.StrAtom "2", Ast.StrAtom "3", Ast.StrAtom "4"]
        , Ast.List 
          [Ast.StrAtom "1", Ast.StrAtom "2", Ast.StrAtom "7", Ast.StrAtom "8"]
        , Ast.List 
          [Ast.StrAtom "5", Ast.StrAtom "6", Ast.StrAtom "3", Ast.StrAtom "4"]
        , Ast.List 
          [Ast.StrAtom "5", Ast.StrAtom "6", Ast.StrAtom "7", Ast.StrAtom "8"]
        ]



-- GET


coreGet :: Spec
coreGet = do

  it "get" $ 
    Core.get [1,2] ["one","two","three","four"] `shouldBe` ["two","three"]



-- SELECT


coreSelect :: Spec
coreSelect = do

  it "select" $
    Core.select [1,2]
      [ ["one","two","three","four"]
      , ["five","six","seven","eight"]
      , ["nine","ten","eleven","twelve"]
      ] `shouldBe`
      [ ["two","three"]
      , ["six","seven"]
      , ["ten","eleven"]
      ]



-- ZIP


corePairUp :: Spec
corePairUp = do

  it "pairUp" $
    Core.pairUp
      [ ["one","two"]
      , ["five","six"]
      ]
      [ ["three","four"]
      , ["seven","eight"]
      , ["eleven","twelve"]
      ] `shouldBe`
      [ (["one","two","three","four"])
      , (["five","six","seven","eight"])
      ]



-- MERGE


coreMerge :: Spec
coreMerge = do

  it "merge" $
    Core.merge
      ["one","","","four"]
      ["","six","","eight"] `shouldBe`
      ["one","six","","four"]
      