module ViktorTest where

import Test.Tasty.Hspec

import qualified Viktor



-- PROBLEM 1


problem1 :: Spec
problem1 = do

  it "example 1" $
    Viktor.p1 
      [ ["Jian","Shi"] ]
      [ ["Julian","Rathke"] ] `shouldBe` 
      [ ["Jian","Shi","Julian","Rathke"] ]

  it "example 2" $
    Viktor.p1 
      [ ["1","2"] 
      , ["1","2"]
      ]
      [ ["3","4"] 
      , ["3","4"]
      ] `shouldBe`
      [ ["1","2","3","4"] 
      , ["1","2","3","4"]
      , ["1","2","3","4"]
      , ["1","2","3","4"]
      ]

  it "example 3" $
    Viktor.p1 [] [ ["Haskell","Java"] ] `shouldBe` []