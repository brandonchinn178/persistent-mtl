import Test.Tasty

import qualified Integration
import qualified Mocked

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "persistent-mtl"
  [ Mocked.tests
  , Integration.tests
  ]
