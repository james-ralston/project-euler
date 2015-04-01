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
solutionsTo50_ n 
  | n == 1 = solution1
  | n == 2 = solution2
  | n == 3 = solution3
  | n == 4 = solution4
  | n == 5 = solution5
  | n == 6 = solution6
  | n == 8 = solution8
  | n == 13 = solution13
  | n == 18 = solution18
  | otherwise = print "solution not present in program"

solution1 = putStrLn $ show answer
  where answer = sum $ union [3,6..999] [5,10..999]
    
solution2 = putStrLn $ show answer
  where answer = sum . filter even $ takeWhile (<= 4000000) fibs
  
solution3 = putStrLn $ show answer
  where answer =  let primes = 2 : filter (null . tail . primeFactors) [3,5..]
                      primeFactors n = factor n primes
                        where factor n (p:ps) 
                                | p*p > n        = [n]
                                | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                                | otherwise      =     factor n ps
                  in  maximum $ primeFactors 600851475143
                  
solution4 = putStrLn $ show answer
  where answer = maximum [x | y<-[999,998..100], z<-[999,998..y], let x = y*z, isPalindromic x]

solution5 = putStrLn $ show answer
  where answer = foldr1 lcm [1..20]
  
solution6 = putStrLn $ show answer
  where answer = sumSquare - squareSums
          where squareSums =  foldl (\s i-> s + i^2) 0 [1..100]
                sumSquare = (^2) $ sum [1..100]
                
solution8 = putStrLn $ show answer 
  where answer = getAdjacentSums digitArr 13
          where getAdjacentSums [] _ = 0
                getAdjacentSums xss@(x:xs) y
                  | len < y = 0
                  | len == y = product xss      
                  | otherwise = max adjSum $ getAdjacentSums xs y
                  where len = length xss
                        adjSum = product $ take y xss
                digitArr = map digitToInt digitString

solution13 = do
  numbers <- fmap ( map read . lines ) (readFile "p13numbers.txt")                
  putStrLn . take 10 . show . sum $ numbers
  

  
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
