{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.List
import System.Random
import QuickSort (qSort)
import Control.Exception
import Control.Monad.Par


-- from http://www.haskell.org/haskellwiki/Examples/Random_list
randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

exeMain = do{-
    seed  <- newStdGen
    let rs = randomlist (1000*1000) seed
    print rs
    let ys = qSort rs
    print ys
        -}
    -- split a list in two lists and sort parallel.
    let n = 1000*1000
    seed  <- newStdGen
    let rs = randomlist n seed
    let x = take (n/2) rs
    let y = drop (n/2) rs
    print $ runPar $ do
        fx <- qSort x    -- start evaluating (f x)
        gx <- qSort y    -- start evaluating (g x)
        a <- get fx             -- wait for fx
        b <- get gx             -- wait for gx
        return qSort (a++b)     -- return results

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION
