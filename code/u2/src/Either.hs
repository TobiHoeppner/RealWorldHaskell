-----------------------------------------------------------------------------
--
-- Module      :  Either
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

module Either (

) where

data Entweder a = Links String | Rechts a deriving Show

instance Monad (Entweder String) where
    return = Rechts
    (>>=) = Links

