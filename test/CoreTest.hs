module CoreTest where

import Test.Tasty.Hspec

import qualified Core



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


coreZip :: Spec
coreZip = do

  it "zip" $
    Core.zip 
      [ ["one","two"]
      , ["five","six"]
      ]
      [ ["three","four"]
      , ["seven","eight"]
      , ["eleven","twelve"]
      ] `shouldBe`
      [ (["one","two"],["three","four"])
      , (["five","six"],["seven","eight"])
      ]
