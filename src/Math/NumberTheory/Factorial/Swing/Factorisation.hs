{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fast factoroial function by DSC method, utilising prime factorisation
module Math.NumberTheory.Factorial.Swing.Factorisation (factorial, fastFactorial) where

import Control.Comonad
import qualified Control.Foldl as L
import Control.Monad (guard)
import Control.Monad.ST
import Data.Bits (Bits (popCount), shiftL)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search
import qualified Math.NumberTheory.Factorial.Naive as Naive
import Math.NumberTheory.Powers.Squares (integerSquareRoot')
import Math.NumberTheory.Primes

-- >>> fastFactorial 10
-- 3628800

primesTo :: Integer -> V.Vector (Prime Integer)
primesTo = V.fromList . enumFromTo (precPrime 2) . precPrime

fastFactorial :: Integer -> Integer
fastFactorial n
  | n < 10 = Naive.factorial' n
  | otherwise =
    let bs = fromIntegral n - popCount n
     in fastOddFactorial n (primesTo n) `shiftL` bs

fastOddFactorial :: Integer -> V.Vector (Prime Integer) -> Integer
fastOddFactorial n ps
  | n < 2 = 1
  | otherwise =
    fastOddFactorial (n `div` 2) (fst $ bsplitOn (1 + (n `div` 2)) ps) ^ 2
      * swingFast n ps

-- >>> swingFast 2 (primesTo 2)
-- 1
swingFast :: Integer -> V.Vector (Prime Integer) -> Integer
swingFast 0 _ = 1
swingFast 1 _ = 1
swingFast 2 _ = 1
swingFast 3 _ = 3
swingFast m ps =
  let (inis, restS) = bsplitOn (1 + integerSquareRoot' m) ps
      (interms, _) = bsplitOn (1 + m `div` 3) restS
      (_, largePrimes) = bsplitOn (1 + m `div` 2) ps
   in L.fold
        ( L.premap (swingLoop m) $
            L.handles L.folded $
              L.fold
                ( L.prefilter (odd . div m . unPrime) $
                    L.premap unPrime $
                      duplicate
                        ( L.fold
                            (L.premap unPrime $ duplicate L.product)
                            largePrimes
                        )
                )
                interms
        )
        $ V.tail inis

-- >>> :t L.folded . mapped unPrime

bsearch :: Ord a => a -> V.Vector (Prime a) -> Int
bsearch a v = runST $ do
  mv <- V.unsafeThaw v
  binarySearchP ((>= a) . unPrime) mv

bsplitOn :: Ord a => a -> V.Vector (Prime a) -> (V.Vector (Prime a), V.Vector (Prime a))
bsplitOn a = V.splitAt <$> bsearch a <*> id

factorial :: Integer -> Integer
factorial n
  | n < 2 = 1
  | otherwise = factorial (n `div` 2) ^ 2 * primeSwing n

primeSwing :: Integer -> Integer
primeSwing n =
  foldl' (flip (maybe id (*) . swingLoop n)) 1 $
    primesTo n

-- >>> swingLoop 10 (precPrime 3)
-- Just 9

swingLoop :: Integer -> Prime Integer -> Maybe Integer
swingLoop n (unPrime -> prime) = go n 1
  where
    go !q !p =
      let q' = q `div` prime
          p'
            | odd q' = p * prime
            | otherwise = p
       in if q' == 0
            then do
              guard $ p /= 1
              pure p
            else go q' p'
