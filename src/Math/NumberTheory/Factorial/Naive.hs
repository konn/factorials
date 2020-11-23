{-# LANGUAGE BangPatterns #-}

{- | Provides a naïve factorial function, just
   using list or strict recursion to takes product of it.
-}
module Math.NumberTheory.Factorial.Naive where

import Data.List (foldl')

-- | O(n)
factorialList :: (Num a, Enum a) => a -> a
factorialList = foldl' (*) 1 . enumFromTo 1

-- | O(n), factorial with strict recursion with accumulator
factorial' :: (Ord a, Num a) => a -> a
factorial' = loop 1
  where
    loop !acc !n
      | n <= 0 = acc
      | otherwise = loop (n * acc) (n - 1)

-- >>> factorial' 10
-- 3628800

-- >>> factorialList 100
-- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

-- >>> factorial' 100
-- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
