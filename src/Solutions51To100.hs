module Solutions51To100
( solutions51To100
) where


import System.IO
import Data.List
import Control.Applicative
import MyMath.Fibonacci
import Util.Misc (isPalindromic)
import Data.Char
import Data.Numbers.Primes
import Data.Ord
import Data.Function


solutions51To100 :: Int -> IO ()
solutions51To100 n = do 
  putStr $ "The solution to problem " ++ show n ++ " is "
  solutions51To100_ n

solutions51To100_ :: Int -> IO ()
solutions51To100_ n 
  | n == 58 = solution58
  | n == 92 = solution92
  | n == 94 = solution94
  | n == 97 = solution97
  | n == 347 = solution347
  | otherwise = print "solution not present in program"

  
solution92 = putStrLn . show . sum $ map solver [1..hi]
  where solver n 
          | n == 1 = 0
          | n == 89 = 1
          | otherwise = solver . sum . map ((^2) . readInt ) $ show n
          where readInt n = read [n] :: Int
        hi = 10000000
        
solution58 = putStrLn . show . fst $ head  answer
  where answer = take 1 $ dropWhile lol func
          where func = scanl1 (\t b -> (fst b, snd t + snd  b)) . map (\s -> (s, foo s)) $  filter odd [3..]
                  where foo n =  length . filter isPrime . take 4 $ iterate (\i -> i - n + 1) (n^2)
                lol n = if ff n > 0.1 then True else False
                  where ff (a,b) = (fromIntegral b) / ((2 *(fromIntegral a)) - 1)
      
solution97 = putStrLn . drop 2357197 . show $ 28433 * 2^7830457 + 1

isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

solution94 = putStrLn . show . sum $ map func [2..333333333]
  where func x 
          | tbb = 6 * x
          | tgb = 3 * x + 1
          | tlb = 3 * x - 1
          | otherwise = 0
          where n = fromIntegral x
                sp  = (3 * n ^ 2) - 1
                tg2 = sqrt (sp - 2*n)
                tl2 = sqrt (sp + 2*n)
                tg = (n + 1) * tg2
                tl = (n - 1) * tl2
                tgb = isInt tg2 15
                tlb = isInt tl2 15
                tbb = and [tgb,tlb]

solution347 = 
  putStrLn . show . 
  sum . 
  map (snd . last) . 
  groupBy ((==) `on` fst) . 
  sortBy (comparing fst) $ datums 5000000
    where datums n = map (foo . concat) . filter ((==2) . length) $ map (group . primeFactors) [1..n]
            where foo x = (foo' x , product x)
                    where foo' x = (last x , head x)