import Lecture01TestSuite
import Test.Tasty (TestTree, defaultMain, testGroup)

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Haskell Lecture Series - Shuttl"
    [ lecture01TestSuite
    ]
