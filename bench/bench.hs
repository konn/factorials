module Main where

import Gauge.Main
import qualified Math.NumberTheory.Factorial.Naive as Naive
import qualified Math.NumberTheory.Factorial.Swing.Factorisation as Factor
import qualified Math.NumberTheory.Factorial.Swing.Recursion as Recur

main :: IO ()
main =
  defaultMain
    [ mkBGroup 5
    , mkBGroup 10
    , mkBGroup 25
    , mkBGroup 100
    , mkBGroup 1000
    , mkBGroup 5000
    , mkBGroup 10000
    , mkBGroup 50000
    ]

mkBGroup :: Integer -> Benchmark
mkBGroup n =
  bgroup
    (show n)
    [ bench "Strict, accumulated loop" $ nf Naive.factorial' n
    , bench "Strict left-fold" $ nf Naive.factorial' n
    , bench "DSC with factorisation" $ nf Factor.factorial n
    , bench "DSC with factorisation (optimised)" $ nf Factor.factorial n
    , bench "DSC with recursion" $ nf Recur.factorial n
    ]
