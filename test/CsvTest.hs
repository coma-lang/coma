module CsvTest where

import qualified Csv

import Test.Tasty.Hspec



-- PARSE


spec_csv_parse :: Spec
spec_csv_parse = do

  it "normal CSV" $
    Csv.parse "1,2,3\n4,5,6\n7,8,9" 
        `shouldBe` [["1","2","3"],["4","5","6"],["7","8","9"]]

  it "CSV with missing items" $
    Csv.parse ",2,3\n,,6\n7,8," 
        `shouldBe` [["","2","3"],["","","6"],["7","8",""]]

