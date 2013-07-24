-----------------------------------------------------------------------------
--
-- Module      :  Echo
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

module Echo (
echoServ
) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hClose, hGetLine, hSetBuffering, stdout, BufferMode(..))
import Control.Monad (forever)
import Text.Printf (hPrintf, printf)
import Network (listenOn, accept, Socket (..), PortID (..))
import Network.Socket (close)
import Control.Exception (handle, finally, SomeException)
import Control.Concurrent (forkIO)

echoServ :: IO ()
{-
echoServ' = getArgs >>= return . read . (!! 0)
               >>= listenOn . PortNumber . fromIntegral
               >>= forever . serve
-}
echoServ = do
    args <- getArgs
    let port = ((read . (!! 0)) args) :: Int
    x <- listenOn (PortNumber(fromIntegral port))
    hSetBuffering stdout NoBuffering
    printf "Server auf Port %d gestartet." port
    -- management console
    forkIO $ manage x
    -- socket listener
    forever $ serve x


serve :: Socket -> IO()
serve socket = handle exitSrv $ do
  (sock, host, _) <- accept socket

  forkIO $ flip finally (hClose sock) $ do
    -- hier einlesen vom Socket
    text <- hGetLine sock
    -- hier ausgeben vom Socket
    hPrintf sock text >> hFlush sock
  -- lokale bestätigung
  printf "Anfrage von %s beantwortet\n" host

manage x = do
    line <- getLine
    case line of
        "exit" -> close x
        _ -> manage x

exitSrv :: SomeException -> IO()
exitSrv e = exitFailure
