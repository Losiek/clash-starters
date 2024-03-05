import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import qualified Tests.Example.Adder

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Example.Project.accumTests,
    Tests.Example.Adder.adderTests
  ]
