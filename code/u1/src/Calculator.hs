-----------------------------------------------------------------------------
--
-- Module      :  Calculator
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

module Calculator (
exec
) where
import Tokenize
-- Unsere Strategie ist die folgende:
-- Wir wollen zuerst einen String parsen. Dabei Ã¼berfÃ¼hren wir ihn
-- in ein eigenes Datenformat, 'Expr'. Dies passiert in den Funktionen
-- 'parse' und 'parse'' (sprich /parse prime/). Am Ende wird der
-- Ausdruck ausgewertet via 'eval'.

-- | Eine Expr ist entweder eine Addition (von zwei Expr),
-- eine Subtraktion, eine Division, eine Multiplikation, oder
-- ein Literal (also ein konstanter Wert, z.B. @839@).
data Expr =
      Add Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Mul Expr Expr
    | Lit Float
{-

Alternativ kann 'main' auch so geschrieben werden:

> main' = getArgs >>= (\x -> if null x then showUsage else exec (head x) >>= print)
>   where
>     showUsage = putStrLn "Usage..."

-}


-- | exec FÃ¼hrt die Berechnung in einem String aus und gibt das
-- Ergebnis auf der Konsole aus.
exec :: String -> IO ()
exec s = do
    let e = parse s
    let x = eval e      -- e :: Expr
    putStrLn (show x) -- x :: Float


-- | Parsen ist die HintereinanderausfÃ¼hrung von 'tokenize' und
-- 'parse''.
parse :: String -> Expr
parse = parse' . tokenize


-- | Poor mans parser: Der Parser akzeptiert eine Liste von Tokens
-- (die ihrerseits Strings sind) und parst diese indem er einfach
-- die Liste von Tokens anhand der Operatoren aufbricht,
-- via 'break'.
parse' :: [String] -> Expr
parse' [x] = Lit (read x)
parse' [l,o,r] = case o of
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


-- | Wertet den Ausdruck zu einem Float aus.
eval :: Expr -> Float
eval e = case e of
    Add l r -> eval l + eval r
    Sub l r -> eval l - eval r
    Div l r -> eval l / eval r
    Mul l r -> eval l * eval r
    Lit l   -> l
