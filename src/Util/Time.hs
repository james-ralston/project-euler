-- |
-- Module      : Data.Numbers.Primes
-- Copyright   : Sebastian Fischer
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an efficient lazy wheel sieve for
-- prime generation inspired by /Lazy wheel sieves and spirals of/
-- /primes/ by Colin Runciman
-- (<http://www.cs.york.ac.uk/ftpdir/pub/colin/jfp97lw.ps.gz>) and
-- /The Genuine Sieve of Eratosthenes/ by Melissa O'Neil
-- (<http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>).
-- 
module Util.Time (

  time

  ) where
  
  import System.Environment
  import Text.Printf
  import Control.Exception
  import System.CPUTime
  
  time :: IO t -> IO t
  time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = ( fromIntegral (end - start)) / (10 ^ 12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v


