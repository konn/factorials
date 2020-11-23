module Math.NumberTheory.Factorial.NaiveSpec where

import Math.NumberTheory.Factorial.Naive
import Math.NumberTheory.Logarithms
import Test.Tasty.QuickCheck

prop_factorialList :: NonNegative Int -> Property
prop_factorialList (NonNegative n) =
  tabulate "log_2(N+1)" [show $ intLog2' (n + 1)] $
    factorialList n === product [1 .. n]

prop_factorial' :: NonNegative Int -> Property
prop_factorial' (NonNegative n) =
  tabulate "log_2(N+1)" [show $ intLog2' (n + 1)] $
    factorial' n === product [1 .. n]
