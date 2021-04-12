module ComaTest where

import Test.Tasty.Hspec

import qualified Data.HashMap as HM

import qualified Coma
import qualified Ast



-- READ


comaExecLambda :: Spec
comaExecLambda = do

  it "lambda" $ 5 `shouldBe` 5
