module ComaTest where

import Test.Tasty.Hspec

import qualified Data.HashMap as HM

import qualified Coma
import qualified Ast



-- READ


comaExecLambda :: Spec
comaExecLambda = do

  let lambda = Ast.Lambda HM.empty ["a","b"] (Ast.Ident "b")
  let call = Ast.Call (Ast.Call lambda $ Ast.IntAtom 42) $ Ast.IntAtom 5

  answer <- runIO $ Coma.exec call

  it "lambda" $ answer `shouldBe` Ast.IntAtom 5
