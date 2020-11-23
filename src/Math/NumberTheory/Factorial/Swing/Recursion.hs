{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fast factoroial function by recursive DSC method
module Math.NumberTheory.Factorial.Swing.Recursion (factorial) where

import Data.Bits
import qualified Math.NumberTheory.Factorial.Naive as Naive

-- >>> factorial 10
-- 3628800

factorial :: Integer -> Integer
factorial n
  | n < 10 = Naive.factorial' n
  | otherwise =
    let bc = n - fromIntegral (popCount n)
     in getFst (oddFractional n) * 2 ^ bc

fallingSemifrac :: Integer -> Integer -> Integer
fallingSemifrac !m !p
  | p == 1 = m
  | p == 2 = m * (m - 2)
  | otherwise =
    let hlen = p `div` 2
     in fallingSemifrac (m - hlen * 2) (p - hlen)
          * fallingSemifrac m hlen

data P a = P {getFst :: !a, _getSnd :: !a}

oddFractional :: Integer -> P Integer
oddFractional 0 = P 1 1
oddFractional 1 = P 1 1
oddFractional 2 = P 1 1
oddFractional 3 = P 3 1
oddFractional 4 = P 3 1
oddFractional n =
  let P sqrOddFac oldOddFac = oddFractional $ n `div` 2
      !len0 = (n - 1) `div` 4
      !len
        | (n `mod` 4) == 2 = len0
        | otherwise = len0 + 1
      !high = n - ((n + 1) .&. 1)
      !oddSwing = fallingSemifrac high len `div` oldOddFac
      !oddFac = sqrOddFac ^ 2 * oddSwing
   in P oddFac sqrOddFac
