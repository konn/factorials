{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fast factoroial function by DSC method, utilising prime factorisation
module Math.NumberTheory.Factorial.Swing.Factorisation (factorial) where

import Data.Foldable (Foldable (foldl'))
import Math.NumberTheory.Primes

-- >>> factorial 10
-- 3628800

-- >>> factorial 100
-- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

factorial :: Integer -> Integer
factorial n
  | n < 2 = 1
  | otherwise = factorial (n `div` 2) ^ 2 * primeSwing n

primeSwing :: Integer -> Integer
primeSwing n =
  foldl' (flip ((*) . go n)) 1 $
    takeWhile ((<= n) . unPrime) primes

data Pair x = P {getFst :: !x, getSnd :: !x}

go :: Integer -> Prime Integer -> Integer
go n (unPrime -> prime) = go n 1
  where
    go !q !p =
      let q' = q `div` prime
          p'
            | odd q' = p * prime
            | otherwise = p
       in if q' == 0
            then p'
            else go q' p'
