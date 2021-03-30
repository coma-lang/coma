import Test.Tasty
import Test.Tasty.Hspec

import qualified CsvTest
import qualified WasifTest



-- MAIN


main :: IO ()
main = do

  csvParse <- testSpec "Parse" CsvTest.specCsvParse
  problem2 <- testSpec "#2" WasifTest.problem2
  problem3 <- testSpec "#3" WasifTest.problem3

  defaultMain $
    testGroup "All Tests"
      [ testGroup "CSV"
        [ csvParse ]
      , testGroup "Problems"
        [ problem2
        , problem3
        ]
      ]
