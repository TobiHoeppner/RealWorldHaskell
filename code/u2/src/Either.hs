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

-- (>>=):: (EntwederString a) -> (a -> EntwederString b) -> (EntwederString b)

instance Monad EntwederString where
    return = Rechts
    m >>= f = case m of
        Links s -> Links s
        Recht a -> f a
