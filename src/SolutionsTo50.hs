module SolutionsTo50 
( solutionsTo50 
) where


import System.IO
import Data.List
import Control.Applicative
import MyMath.Fibonacci
import Util.Misc (isPalindromic)
import Data.Char


solutionsTo50 :: Int -> IO ()
solutionsTo50 n = do 
  putStr $ "The solution to problem " ++ show n ++ " is "
  solutionsTo50_ n

solutionsTo50_ :: Int -> IO ()
solutionsTo50_ n = case n of
    1  -> print solution1
    2  -> print solution2
    3  -> print solution3
    4  -> print solution4
    5  -> print solution5
    6  -> print solution6
    8  -> print solution8
    13 -> solution13
    18 -> solution18
    _  -> putStrLn "Solution not present in program"

-- | Solves Project Euler Problem 1:
-- Find the sum of all the multiples of 3 or 5 below 1000.
-- This is solved by taking the union of multiples of 3 and 5 and summing them.
solution1 :: Integer
solution1 = sum $ union [3,6..999] [5,10..999]
    
-- | Solves Project Euler Problem 2:
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million,
-- find the sum of the even-valued terms.
solution2 :: Integer
solution2 = sum . filter even $ takeWhile (<= 4000000) fibs
  
-- | Solves Project Euler Problem 3:
-- Find the largest prime factor of the number 600851475143.
-- This is solved by generating prime factors and finding the maximum.
solution3 :: Integer
solution3 = let primes = 2 : filter (null . tail . primeFactors) [3,5..]
                primeFactors n = factor n primes
                  where factor n (p:ps) 
                          | p*p > n        = [n]
                          | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                          | otherwise      =     factor n ps
            in  maximum $ primeFactors 600851475143
                  
-- | Solves Project Euler Problem 4:
-- Find the largest palindrome made from the product of two 3-digit numbers.
-- This is solved by iterating through products of 3-digit numbers and checking for palindromes.
solution4 :: Integer
solution4 = maximum [x | y<-[999,998..100], z<-[999,998..y], let x = y*z, isPalindromic x]

-- | Solves Project Euler Problem 5:
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-- This is solved by finding the least common multiple (LCM) of numbers from 1 to 20.
solution5 :: Integer
solution5 = foldr1 lcm [1..20]
  
-- | Solves Project Euler Problem 6:
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
solution6 :: Integer
solution6 = sumSquare - squareSums
  where squareSums = sum $ map (^2) [1..100]
        sumSquare = (^2) $ sum [1..100]
                
-- | Solves Project Euler Problem 8:
-- Find the thirteen adjacent digits in the 1000-digit number that have the greatest product.
solution8 :: Integer
solution8 = maximum . map product . takeWhile (\l -> length l >= 13) . map (take 13) . tails $ digitArr
  where digitArr = map digitToInt digitString

-- | Solves Project Euler Problem 13:
-- Work out the first ten digits of the sum of the following one hundred 50-digit numbers
-- (stored in p13numbers.txt).
solution13 = do
  numbers <- fmap ( map read . lines ) (readFile "p13numbers.txt")                
  putStrLn . take 10 . show . sum $ numbers
  

  
-- | Solves Project Euler Problem 18:
-- Find the maximum total from top to bottom of the triangle (stored in triangle.txt).
solution18 = do  
  a <- answer
  putStrLn $ show a
  where answer = head . foldr1 solve' . parse <$> readFile "triangle.txt"
          where parse t = map (map read . words) $ lines t
                solve' [] [z] = [z]
                solve' (x:xs) (y:ys:yss) = x + max y ys : solve' xs (ys:yss)
                
                
                
digitString = "73167176531330624919225119674426574742355349194934"
              ++ "96983520312774506326239578318016984801869478851843"
              ++ "85861560789112949495459501737958331952853208805511"
              ++ "12540698747158523863050715693290963295227443043557"
              ++ "66896648950445244523161731856403098711121722383113"
              ++ "62229893423380308135336276614282806444486645238749"
              ++ "30358907296290491560440772390713810515859307960866"
              ++ "70172427121883998797908792274921901699720888093776"
              ++ "65727333001053367881220235421809751254540594752243"
              ++ "52584907711670556013604839586446706324415722155397"
              ++ "53697817977846174064955149290862569321978468622482"
              ++ "83972241375657056057490261407972968652414535100474"
              ++ "82166370484403199890008895243450658541227588666881"
              ++ "16427171479924442928230863465674813919123162824586"
              ++ "17866458359124566529476545682848912883142607690042"
              ++ "24219022671055626321111109370544217506941658960408"
              ++ "07198403850962455444362981230987879927244284909188"
              ++ "84580156166097919133875499200524063689912560717606"
              ++ "05886116467109405077541002256983155200055935729725"
              ++ "71636269561882670428252483600823257530420752963450"
