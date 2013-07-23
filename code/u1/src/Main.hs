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

import System.Environment (getArgs)
import Data.List (span)
import Calc (calc)
import Calculator (exec)

-- | args ist eine Funktion die Argumente vom Programmaufruf ausliest und eine Funktion foo auf
-- diese Argumente anwendet.
args = do
    x <- getArgs
    return $ map foo x

-- | foo teilt String bei '=' auf und gibt Stücke jeweils als Tupel zurück
-- ist in dem String kein '=' vorhanden, so gibt es den ersten Wert und Nothing als Tupel zurück
foo :: String -> (String , Maybe String)
foo x = let (a,b) = span (/= '=') x  in
    if b == "" then (a, Nothing)
    else (a, Just $ tail b)

-- | main liest Programmparameter ein.
{-main = do
    --argumente <- args
    --print argumente
    args <- getArgs
    case args of
        [] -> putStrLn "no args"
        (x:xs) -> calc x
-}

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "usage: ..."
        (x:xs) -> do
            res <- calc x
            result <- exec x
            print result
