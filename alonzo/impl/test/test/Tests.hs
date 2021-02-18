module Main where

import Test.Cardano.Ledger.Alonzo.Examples.Mir (mirExamples)
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests,
      mirExamples
    ]

main :: IO ()
main = defaultMain tests
