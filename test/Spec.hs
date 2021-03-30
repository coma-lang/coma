import Test.Tasty
import Test.Tasty.Hspec

import qualified CsvTest
import qualified ViktorTest
import qualified WasifTest
import qualified TimTest



-- MAIN


main :: IO ()
main = do

  csvParse <- testSpec "Parse" CsvTest.csvParse
  csvSerialize <- testSpec "Parse" CsvTest.csvSerialize
  problem1 <- testSpec "#1" ViktorTest.problem1
  problem2 <- testSpec "#2" WasifTest.problem2
  problem3 <- testSpec "#3" WasifTest.problem3
  problem4 <- testSpec "#4" TimTest.problem4
  problem5 <- testSpec "#5" TimTest.problem5

  defaultMain $
    testGroup "All Tests"
      [ testGroup "CSV"
        [ csvParse ]
      , testGroup "Problems"
        [ problem1
        , problem2
        , problem3
        , problem4
        , problem5
        ]
      ]
