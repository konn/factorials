module Math.NumberTheory.Factorial.Swing.FactorisationSpec where

import Math.NumberTheory.Factorial.Swing.Factorisation
import Math.NumberTheory.Logarithms
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

prop_factorial :: NonNegative Integer -> Property
prop_factorial (NonNegative n) =
  tabulate "log_2(N+1)" [show $ integerLog2' (n + 1)] $
    factorial n === product [1 .. n]

test_factorial :: TestTree
test_factorial =
  testGroup
    "factorial (regressions)"
    [ testCase (show n) $
      factorial n @?= product [1 .. n]
    | n <- [10, 100, 1000, 5000, 10000, 50000]
    ]

prop_fastFactorial :: NonNegative Integer -> Property
prop_fastFactorial (NonNegative n) =
  tabulate "log_2(N+1)" [show $ integerLog2' (n + 1)] $
    fastFactorial n === product [1 .. n]

test_fastFactorial :: TestTree
test_fastFactorial =
  testGroup
    "fastFactorial (regressions)"
    [ testCase (show n) $
      fastFactorial n @?= product [1 .. n]
    | n <- [10, 100, 1000, 5000, 10000, 50000]
    ]
