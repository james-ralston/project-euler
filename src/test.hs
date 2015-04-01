module Test where

import Data.Ratio
import Data.Numbers.Primes

getFrac :: (Integral a, Fractional b) => Ratio a -> b
getFrac x = a / b
  where a = fromIntegral $ numerator x
        b = fromIntegral $ denominator x

p :: Triangle -> Double
p (Triangle a b c) = fromIntegral (a + b + c ) 


data Triangle = Triangle Int Int Int

foo n = tl + tg
  where tl = area $ Triangle n n (n-1)
        tg = area $Triangle n n (n + 1)
        
area :: Triangle -> Int
area t@(Triangle a b c) 
  | isInt area' 10 = floor $ area' 
  | otherwise = 0
  where p' = p t 
        a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        area' = sqrt $ p' * ( p' - a') * (p' - b') * (p' - c') 

isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

main = print . sum $ map func [1..3333333]

func x 
  | tbb = 6 * n
  | tgb = 3 * n + 1
  | tlb = 3 * n - 1
  | otherwise = 0
  where n = fromIntegral x
        sp  = (3 * n ^ 2) - 1
        tg2 = sqrt (sp - 2*n)
        tl2 = sqrt (sp + 2*n)
        tg = (n + 1) * tg2
        tl = (n - 1) * tl2
        tgb = isInt tg2 8
        tlb = isInt tl2 8
        tbb = and [tgb,tlb]
       
