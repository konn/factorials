module Math.NumberTheory.Factorial.Swing.RecursionSpec where

import Math.NumberTheory.Factorial.Swing.Recursion
import Math.NumberTheory.Logarithms
import Test.Tasty.QuickCheck

prop_factorial :: NonNegative Integer -> Property
prop_factorial (NonNegative n) =
  withMaxSuccess 200 $
    tabulate "1 + log_10(N+1)" [show $ 1 + integerLog10' (n + 1)] $
      factorial n === product [1 .. n]

prop_factorial_Large :: NonNegative (Large Int) -> Property
prop_factorial_Large (NonNegative (Large n)) =
  withMaxSuccess 25 $
    tabulate "1 + log_10(N+1)" [show $ 1 + integerLog10' (fromIntegral n + 1)] $
      factorial (fromIntegral n) === product [1 .. fromIntegral n]
