import Test.Tasty
import Test.Tasty.Hspec

import qualified CsvTest
import qualified CoreTest



-- MAIN


main :: IO ()
main = do

  csvParse <- testSpec "Parse" CsvTest.csvParse
  csvSerialize <- testSpec "Parse" CsvTest.csvSerialize

  defaultMain $
    testGroup "All Tests"
      [ testGroup "CSV"
        [ csvParse ]
      ]
