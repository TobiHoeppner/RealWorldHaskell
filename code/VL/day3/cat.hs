module Main where

import Control.Monad
import System.IO

-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()
-- print :: Show a => a -> IO ()
-- getLine :: IO String

-- hPutStr
-- hPutStrLn

main = forever (hGetLine stdin >>= hPutStrLn stdout)



