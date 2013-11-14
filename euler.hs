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
problem4 = maximum [x | y <- [999, 998..100], z <- [999, 998..y],
                    let x = y * z, numIsPalindrome x]

-- Problem 5
-- Borrowed from Wiki (LCM function)
problem5 :: Integer
problem5 = foldr1 lcm [1..20]

-- Problem 6
sumSquares :: [Integer] -> Integer
sumSquares = foldr (\x acc -> acc + x * x) 0

squareSum :: [Integer] -> Integer
squareSum xs = s * s
  where s = sum xs

diffSumSquare :: [Integer] -> Integer
diffSumSquare xs = squareSum xs - sumSquares xs

problem6 :: Integer
problem6 = diffSumSquare [1..100]

-- Problem 7
problem7 :: Integer
problem7 = primes !! 10000

-- Problem 8
chrToInt :: Char -> Int
chrToInt c = read [c]

-- From tail function
consecutives :: [a] -> Int -> [[a]]
consecutives xs n = (take n xs) : case xs of
  [] -> []
  _ : xs' -> consecutives xs' n

maxProduct :: [Char] -> Int -> Int
maxProduct s c = maximum $ map charProduct cConsecutives
  where charProduct xs = product $ map chrToInt xs
        cConsecutives = filter ((==5).length) $ consecutives s c

numberString :: String
numberString ="\
\73167176531330624919225119674426574742355349194934\
\96983520312774506326239578318016984801869478851843\
\85861560789112949495459501737958331952853208805511\
\12540698747158523863050715693290963295227443043557\
\66896648950445244523161731856403098711121722383113\
\62229893423380308135336276614282806444486645238749\
\30358907296290491560440772390713810515859307960866\
\70172427121883998797908792274921901699720888093776\
\65727333001053367881220235421809751254540594752243\
\52584907711670556013604839586446706324415722155397\
\53697817977846174064955149290862569321978468622482\
\83972241375657056057490261407972968652414535100474\
\82166370484403199890008895243450658541227588666881\
\16427171479924442928230863465674813919123162824586\
\17866458359124566529476545682848912883142607690042\
\24219022671055626321111109370544217506941658960408\
\07198403850962455444362981230987879927244284909188\
\84580156166097919133875499200524063689912560717606\
\05886116467109405077541002256983155200055935729725\
\71636269561882670428252483600823257530420752963450"

problem8 = maxProduct numberString 5
