-----------------------------------------------------------------------------
--
-- Module      :  Tokenize
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

module Tokenize(tokenize,operator) where

-- |Die `tokenize` Funktion zerlegt einen Eingabestring
-- in eine geordnete Liste von Operanden und Operatoren
tokenize :: String -> [String]
tokenize = tokenize' [] where
            tokenize' a [] = [a]
            tokenize' a (x:xs)
                    | operator x = a:[x]:(tokenize' [] xs)
                    | otherwise = tokenize' (x:a) xs

operator :: Char -> Bool
operator = (`elem` ['+','-','*','/'])
