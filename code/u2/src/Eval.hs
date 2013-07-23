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
-- | nicht ausführbar, nur Beispiele zum Transfer von bind in do-Notation
--
-----------------------------------------------------------------------------

module Eval (
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
a = headsafe [] >>= return . (+1)

a = headsafe [] >>= (\a -> (return . (+1))a)

a' = do
    b <- headSafe[]
    return $ b + 1

f = do
    x
    c <- z
    let g z = z
        g 'c'

f' = x >> z >>= (\c -> let g z = z in g 'c')

z = do
    a
    b
z' = a >> b


g = do
    x1 <- a
    x2 <- b
    let x = 8
    c
    d

g' = a >>= (\x1 -> b >>= (\x2 -> let x = 8 in c >> d))

p = a >>= (\x1 -> b >>= (\x2 -> c >>= (\x3 -> f x2 x3)))

p' = do
    x1 <- a
    x2 <- b
    x3 <- c
    f x2 x3
