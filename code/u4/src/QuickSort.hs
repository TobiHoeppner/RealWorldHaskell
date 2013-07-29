-----------------------------------------------------------------------------
--
-- Module      :  QuickSort
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

module QuickSort (
qSort
) where

-- qSort:: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = [y | y <- xs, y < x] ++ [x] ++ [z | z <- xs, z >= x]

