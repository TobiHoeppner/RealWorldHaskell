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

-- | exportierte Funktion. Kann sehr einfache Mathematische Ausdrücke aus einem String parsen
-- und berechnen
calc :: String -> Float
calc x = do
    let e = parse x
    let i = calc e
    putStrLn(show i)

-- | erzeugt aus einem String eine Expression
parse :: String -> Expr
parse s = parse' (tokenize s)

-- | nicht sichtbare Fallunterscheidung
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

-- | erzeugt aus einer Expression wieder eine Expression oder ein Float
eval :: Expr -> Float
eval e = case e of
    Add l r -> calc l  + calc r
    Sub l r -> calc l  - calc r
    Div l r -> calc l  / calc r
    Mul l r -> calc l  * calc r
    Const v -> v

