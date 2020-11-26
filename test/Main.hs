import Test.Tasty

import qualified Integration
import qualified MockSqlQueryT
import qualified Mocked
import qualified SqlQueryRepTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "persistent-mtl"
  [ Mocked.tests
  , Integration.tests
  , MockSqlQueryT.tests
  , SqlQueryRepTest.tests
  ]
