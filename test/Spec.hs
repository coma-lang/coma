import Test.Tasty
import Test.Tasty.Hspec

import qualified CsvTest



-- MAIN


main :: IO ()
main = do

  csvParse <- testSpec "Parse" CsvTest.csvParse

  defaultMain $
    testGroup "All Tests"
      [ testGroup "CSV"
        [ csvParse ]
      ]
