module WasifTest where

import Test.Tasty.Hspec

import qualified Wasif



-- PROBLEM 2


problem2 :: Spec
problem2 = do

  it "example 1" $
    Wasif.p2 
      [ ["Steve","Steven","Butcher"]
      , ["Dudley","Dudley","Timms"]
      , ["Gillian","Gillian","Carter"]
      ] `shouldBe` 
      [ ["Carter","Gillian"]
      , ["Timms","Dudley"]
      ]

  it "example 2" $
    Wasif.p2 
      [ ["1","3","6"]
      , ["1","2","2"]
      , ["2","2","2"]
      , ["2","2","6"]
      ] `shouldBe` 
      [ ["2","2"]
      , ["6","2"]
      ]

  it "example 3" $
    Wasif.p2 
      [ ["1","1",""]
      , ["1","1","2"]
      , ["5","3","2"]
      , ["5","5","1"]
      , ["","","5"]
      ] `shouldBe` 
      [ ["","1"]
      , ["1","5"]
      , ["2","1"]
      , ["5",""]
      ]



-- PROBLEM 3


problem3 :: Spec
problem3 = do

  it "example 1" $
    Wasif.p3 
      [ ["1","5","4",""]
      , ["2","","2",""]
      , ["3","7","1","2"]
      , ["4","8","",""]
      ]
      [ ["1","6","4","7"]
      , ["2","8","5","3"]
      , ["2","","","1"]
      , ["4","","2","3"]
      ] `shouldBe` 
      [ ["1","5","4","7"]
      , ["2","","2","1"]
      , ["2","8","2","3"]
      , ["4","8","2","3"]
      ] 

  it "example 2" $
    Wasif.p3 
      [ ["Alice","L","","LC"]
      , ["Bob","","CC","CD"]
      ]
      [ ["Bob","C","",""]
      , ["Alice","","AAW",""]
      ] `shouldBe` 
      [ ["Alice","L","AAW","LC"]
      , ["Bob","C","CC","CD"]
      ] 

  it "example 3" $
    Wasif.p3 
      [ ["1","6","2","3"]
      , ["2","7","4","5"]
      ]
      [ ["3","8","6","6"]
      , ["4","9","5","3"]
      ] `shouldBe` 
      [] 