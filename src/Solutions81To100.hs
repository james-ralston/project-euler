module Solutions51To100
( solutions51To100
) where


import System.IO
import Data.List
import Control.Applicative
import MyMath.Fibonacci
import Util.Misc (isPalindromic)
import Data.Char
import Control.Parallel.Strategies
import Control.Parallel
import Data.Maybe


solutions51To100 :: Int -> IO ()
solutions51To100 n = do 
  putStr $ "The solution to problem " ++ show n ++ " is "
  solutions81To100_ n

solutions51To100_ :: Int -> IO ()
solutions51To100_ n 
  | n == 92 = solution92
  | otherwise = print "solution not present in program"

  
solution92 = putStrLn . show . sum $ map solver [1..hi]
  where solver n 
          | n == 1 = 0
          | n == 89 = 1
          | otherwise = solver . sum . map ((^2) . readInt ) $ show n
          where readInt n = read [n] :: Int
        hi = 10000000
        
solution58 = putStrLn $ show answer
  where answer = take 1 $ dropWhile lol func
          where func = scanl1 (\t b -> (fst b, snd t + snd  b)) . map (\s -> (s, foo s)) $  filter odd [1..]
                  where foo =  length . filter isPrime . take 4 $ iterate (\i -> i - n + 1) (n^2)
                lol n = if ff n > 0.1 then False else True
                  where ff (a,b) = (fromIntegral b) / ((2 *(fromIntegral a)) - 1)