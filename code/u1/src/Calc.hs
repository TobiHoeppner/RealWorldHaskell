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
import System.Environment

data Expr = Const Float | Add Expr Expr | Mul Expr Expr | Div Expr Expr | Sub Expr Expr

-- | exportierte Funktion. Kann sehr einfache Mathematische Ausdrücke aus einem String parsen
-- und berechnen
calc :: String -> IO()
calc x = do
    let e = parse x
    let i = eval e
    putStrLn (show i)

-- | erzeugt aus einem String eine Expression
parse :: String -> Expr
parse = parse' . tokenize

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
        (l1, r1) = break (\x -> x == "+" || x == "-") xs
        (l2, r2) = break (\x -> x == "*" || x == "/") xs

-- | erzeugt aus einer Expression wieder eine Expression oder ein Float
eval :: Expr -> Float
eval e = case e of
    Add l r -> eval l  + eval r
    Sub l r -> eval l  - eval r
    Div l r -> eval l  / eval r
    Mul l r -> eval l  * eval r
    Const v -> v

