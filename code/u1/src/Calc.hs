-----------------------------------------------------------------------------
--
-- Module      :  Calc
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

module Calc (
    calc
) where

import Tokenize

data Expr = Const Float | Add Expr Expr | Mul Expr Expr | Div Expr Expr | Sub Expr Expr

calc :: String -> Float
calc x = do
    let e = prase x
    let i = calc e
    putStrLn(show i)

parse :: String -> Expr
parse s = parse' tok
    where
        tok = tokenize s

parse' :: [String] -> Expr
parse' [x] = Const (read x)
parse' [l, o, r] = case o of
    "+" -> Add (parse' [l]) (parse' [r])
    "-" -> Sub (parse' [l]) (parse' [r])
    "*" -> Mul (parse' [l]) (parse' [r])
    "/" -> Div (parse' [l]) (parse' [r])
parse' xs
    | r1 /= [] = (if head r1 == "+" then Add else Sub) (parse' l1) (parse' $ tail r1)
    | r2 /= [] = (if head r2 == "*" then Mul else Div) (parse' l2) (parse' $ tail r2)
    where
        (l1, r1) = break (\x -> x == "+" || "-") xs
        (l1, r1) = break (\x -> x == "*" || "/") xs

-- |
eval :: Expr -> Float
eval = case e of
    Add l r -> calc l  + clac r
    Sub l r -> calc l  - clac r
    Div l r -> calc l  / clac r
    Mul l r -> calc l  * clac r
    Const v -> v

