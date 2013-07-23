-----------------------------------------------------------------------------
--
-- Module      :  Eval
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

module Eval (
evalM, evalM'
) where

{-
m >>= \x -> f x

wird umgeschrieben in

do x <- m
    return f x
-}
evalM :: Monad m => Expr -> m Float
evalM = evalExpr fC fA fD
    where
        fC = return
        fA m1 m2 = m1 >>= (\x -> m2 >>= (\y -> ... {- kommt auf die Monade an -})
        fD m1 m2 = m1 >>= (\x -> m2 >>= (\y -> ... {- kommt auf die Monade an -})

evalM' :: Monad m => Expr -> m Float
evalM' = evalExpr fC fA fD
    where
        fC = return
        fA m1 m2 = do
                    x <- m1
                    y <- m2
                    return fA x y {- kommt auf die Monade an -}
        fD m1 m2 = do
                    x <- m1
                    y <- m2
                    return ... {- kommt auf die Monade an -}

headsafe :: [a] -> Maybe a
headsafe [] >>= return . (+1)

headsafe' :: [a] -> Maybe a
(headsafe' [])

f = do
    x
    c <- z
    let g z = z
        g 'c'

f' = x >> z >>= (\c -> let g z = z in g 'c')
