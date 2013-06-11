module Euler where

import Control.Applicative

-- Problem 1
hasFactor num factor = num `mod` factor == 0
filterMultiples factor = filter (flip hasFactor factor)
filter3xs = filterMultiples 3
filter5xs = filterMultiples 5
-- ^ Oops, not really needed

anyIsFactor :: [Integer] -> Integer -> Bool
anyIsFactor = flip (any.hasFactor)

filterMultiMultiples factors = filter (anyIsFactor factors)
filter3or5xs = filterMultiMultiples ([3, 5] :: [Integer])

sum3or5xsUnder1000 = sum(filter3or5xs [1..999]) -- 233168
-- Project Euler's solution:
-- http://projecteuler.net/project/resources/001_c619a145e9d327a5c4c84649bec9981b/001_overview.pdf
sumDivisibleBy :: Integral a => a -> a -> a
sumDivisibleBy n target = n * (p * (p + 1)) `div` 2
  where p = target `div` n
problem1 :: Integer
problem1 =  sumDivisibleBy'(3) + sumDivisibleBy'(5) - sumDivisibleBy'(15)
  where sumDivisibleBy' = (flip sumDivisibleBy 1000)

-- Problem 2
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem2 :: Integer
problem2 = sum [x | x <- takeWhile (<= 4000000) fibs, even x]

-- Problem 3
-- Borrowed from haskell wiki
primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

problem3 :: Integer
problem3 = last $ primeFactors 600851475143

-- Problem 4
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (_:[]) = True
isPalindrome (x:xs)
  | sameEnds = isPalindrome(subArray)
  | otherwise = False
  where subArray = take (length xs - 1) $ xs
        sameEnds = x == last(xs)

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = xs == reverse(xs)

numIsPalindrome :: Integer -> Bool
numIsPalindrome = isPalindrome.show.abs

prevPalindrome :: Integer -> Integer
prevPalindrome num
  | numIsPalindrome(prevNumber) = prevNumber
  | otherwise = prevPalindrome(prevNumber)
  where prevNumber = num - 1

problem4 :: Integer
problem4 = maximum [x | y <- [999, 998..100], z <- [999, 998..100],
                    let x = y * z, numIsPalindrome x]
