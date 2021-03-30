module TimTest where

import Test.Tasty.Hspec

import qualified Tim



-- PROBLEM 4


problem4 :: Spec
problem4 = do

  it "example 1" $
    Tim.p4
      [ ["Michael","Baker"]
      , ["Leonard","Hunt"]
      , ["Oscar","Walker"]
      ] `shouldBe` 
      [ ["Leonard","Hunt"]
      , ["Michael","Baker"]
      , ["Oscar","Walker"]
      ]

  it "example 2" $
    Tim.p4
      [ ["David","Beckham"]
      , ["Pele",""]
      , ["Diego","Maradona"]
      , ["Cristiano","Ronaldo"]
      , ["Ronaldinho",""]
      ] `shouldBe` 
      [ ["Cristiano","Ronaldo"]
      , ["David","Beckham"]
      , ["Diego","Maradona"]
      ]



-- PROBLEM 5


problem5 :: Spec
problem5 = do

  it "example 1" $
    Tim.p5
      [ ["Steven"]
      , ["Dudley"]
      , ["Gillian"]
      ] `shouldBe` 
      [ ["Dudley","0","Dudley"]
      , ["Gillian","0","Gillian"]
      , ["Steven","0","Steven"]
      ] 