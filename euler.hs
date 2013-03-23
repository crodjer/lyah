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
sumDivisibleBy n target = n * (p * (p + 1)) `div` 2
  where p = target `div` n
sum3or5xsUnder1000' =  sumDivisibleBy'(3) + sumDivisibleBy'(5) - sumDivisibleBy'(15)
  where sumDivisibleBy' = (flip sumDivisibleBy 1000)

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
