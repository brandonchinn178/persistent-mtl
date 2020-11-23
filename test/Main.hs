import Test.Tasty

import qualified Mocked

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "persistent-mtl"
  [ Mocked.tests
  ]
