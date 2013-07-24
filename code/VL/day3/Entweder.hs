module Entweder where

data Entweder a = Links String | Rechts a
    deriving Show

{-
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    fail :: String -> m a
-}


instance Monad Entweder where
    return = Rechts
    m >>= f = case m of
        Rechts wert -> f wert
        Links str -> Links str

    fail str = Links str

data Id a = Id a

instance Monad Id where
    return = Id
    (Id x) >>= f = f x

-- print :: Show a => a -> IO ()

{-
main :: IO ()
main = do
    let xss = ["Hallo", "Welt", "da draußen"]
    mapM :: (a -> m b) -> [a] -> m [b]
    mapM print xss

    forM :: [a] -> (a -> m b) -> m [b]
    forM xss $ do
        ...
        ...
        ...
    putStrLn "xxx"
-}

{-

Keine Monaden:

id, map, ...

Mit Monaden:

return, mapM

-}


{-
instance Monad Maybe where

    return = Just
    m >>= f = case m of
        Just wert -> f wert
        _ -> Nothing

    fail _ = Nothing
-}

f :: Monad m => Int -> m Int
f i = if i == 0 then fail "Null ist nicht erlaubt" else return i


