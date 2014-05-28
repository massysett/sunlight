-- | This file is an example of how you can use sunlight.  I
-- actually use it to test sunlight itself.
--
-- I keep multiple versions of GHC installed.  Each one can be found
-- in the PATH so there is no need to give exact paths to each one.
module Main where

import Test.Sunlight

ghc s = (s, "ghc-" ++ s, "ghc-pkg-" ++ s)

inputs = TestInputs
  { tiDescription = Nothing
  , tiCabal = "cabal"
  , tiLowest = ghc "7.4.1"
  , tiDefault = map ghc ["7.4.1", "7.6.3", "7.8.2"]
  , tiTest = []
  }

main = runTests inputs
