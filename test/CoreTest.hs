module CoreTest where

import Test.Tasty.Hspec

import qualified Data.HashMap as HM
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

import qualified Core
import qualified Ast



-- MERGE


coreMerge :: Spec
coreMerge = do

  it "merge" $
    Core.merge
      ["one","","","four"]
      ["","six","","eight"] `shouldBe`
      ["one","six","","four"]
      