-- | This file is an example of how you can use sunlight.  I
-- actually use it to test sunlight itself.
--
-- I keep multiple versions of GHC installed.  Each one can be found
-- in the PATH so there is no need to give exact paths to each one.
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
