module Main where

import Control.Monad (forM_)
import qualified Math.NumberTheory.Factorial.Naive as Naive
import qualified Math.NumberTheory.Factorial.Swing.Factorisation as Factor
import qualified Math.NumberTheory.Factorial.Swing.Recursion as Recur
import Weigh

{-# SPECIALIZE Naive.factorial' :: Integer -> Integer #-}

main :: IO ()
main = mainWith $ do
  weighFact "strict accumulated loop" Naive.factorial'
  weighFact "strict left-fold" Naive.factorialList
  weighFact "PrimeSwing" Factor.factorial
  weighFact "PrimeSwing (opt)" Factor.fastFactorial
  weighFact "SplitRecursive" Recur.factorial

weighFact :: [Char] -> (Integer -> Integer) -> Weigh ()
weighFact name fac = wgroup name $
  forM_
    [5 :: Integer, 10, 25, 100, 1000, 5000, 10000, 50000]
    $ \n -> func (show n) fac n
