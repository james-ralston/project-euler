-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) James Ralston
-- License     :  MIT
--
-- Maintainer  :  James Ralston <jpralston1@gmail.com>
-- Stability   :  stable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import System.IO
import Data.List
import Control.Applicative
import Util.Time
import SolutionsTo20 


main = do
  putStrLn "For which problem would you like to see the solution?"
  numStr <- getLine
  let number = checkNumber numStr
  time $ solve number
  where checkNumber s
          | null num' = Nothing
          | otherwise = Just ( fst . head $ num' )
          where num' = reads s :: [(Int,String)]

solve Nothing = print "You have not entered a readable number"
solve (Just x) 
  | x < 20 =  solutionsTo20 x 
solve _ =  print "solution not present in program"



