import Test.Tasty
import Test.Tasty.Hspec

import qualified CsvTest



-- MAIN


main :: IO ()
main = do
  csv_parse <- testSpec "Parse" CsvTest.spec_csv_parse
  defaultMain
    (testGroup "CSV"
      [ csv_parse ]
    )
  
