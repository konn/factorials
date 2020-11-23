module Math.NumberTheory.Factorial.Swing.FactorisationSpec where

import Math.NumberTheory.Factorial.Swing.Factorisation
import Math.NumberTheory.Logarithms
import Test.Tasty.QuickCheck

prop_factorial :: NonNegative Integer -> Property
prop_factorial (NonNegative n) =
  tabulate "log_2(N+1)" [show $ integerLog2' (n + 1)] $
    factorial n === product [1 .. n]
