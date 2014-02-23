module Main where

import Test.Sunlight

inputs = TestInputs
  { tiDescription = Nothing
  , tiCabal = "cabal"
  , tiLowest = ("7.4", "ghc-7.4", "ghc-pkg-7.4")
  , tiDefault = [ ("7.4", "ghc-7.4", "ghc-pkg-7.4")
                , ("7.6", "ghc-7.6", "ghc-pkg-7.6") ]
  , tiTest = []
  }

main = runTests inputs
