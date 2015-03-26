module Util.Misc 
( isPalindromic
) where

isPalindromic :: (Show a) => a -> Bool
isPalindromic n = show n == (reverse . show) n 