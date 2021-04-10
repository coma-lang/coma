import Test.Tasty
import Test.Tasty.Hspec

import qualified CsvTest
import qualified CoreTest
import qualified ComaTest



-- MAIN


main :: IO ()
main = do

  csvParse <- testSpec "Parse" CsvTest.csvParse
  csvSerialize <- testSpec "Parse" CsvTest.csvSerialize

  coreRead <- testSpec "read" CoreTest.coreRead
  coreGet <- testSpec "get" CoreTest.coreGet
  coreSelect <- testSpec "select" CoreTest.coreSelect
  coreJoin <- testSpec "join" CoreTest.coreJoin
  corePairUp <- testSpec "pairUp" CoreTest.corePairUp
  coreMerge <- testSpec "merge" CoreTest.coreMerge

  comaExecLambda <- testSpec "exec" ComaTest.comaExecLambda

  defaultMain $
    testGroup "All Tests"
      [ testGroup "CSV"
        [ csvParse ]
      , testGroup "Core"
        [ coreRead
        , coreJoin
        , coreGet
        , coreSelect
        , corePairUp
        , coreMerge
        ]
      , testGroup "Coma"
        [ comaExecLambda
        ]
      ]
