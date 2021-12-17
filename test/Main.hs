import Test.Tasty

import qualified Basic
import qualified Integration
import qualified MockSqlQueryT
import qualified Mocked
import qualified SqlQueryRepTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "persistent-mtl"
  [ Basic.tests
  , Mocked.tests
  , Integration.tests
  , MockSqlQueryT.tests
  , SqlQueryRepTest.tests
  ]
