-----------------------------------------------------------------------------
--
-- Module      :  KVServer
-- Copyright   :  T. Hoeppner
-- License     :  AllRightsReserved
--
-- Maintainer  :  T. Hoeppner <t.hoeppner@fu-berlin.de>
-- Stability   :  not stable
-- Portability :  not portable
-- | further inspiration: <https://github.com/honza/redish/blob/master/Main.hs>
-- | simple Server that maintains key-value store, port is given by first programm argument
-- protocol:
--  cmd [<k, v>]
-- supported commands
--  add <k,v>
--  rem <k,v>
--  rqk <k>
--  rqv <v>
-----------------------------------------------------------------------------

module KVServer (
kvServ
) where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, hClose, hGetLine, hSetBuffering, stdout, BufferMode(..))
import Control.Monad (forever)
import Text.Printf (hPrintf, printf)
import Network (withSocketsDo ,listenOn, accept, Socket (..), PortID (..))
import Network.Socket (close)
import Control.Exception (handle, finally, SomeException)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Store (add, remove, requestKey, requestVal, initStore)
import Data.Map (Map, fromList)

type Store = Map Int String

kvServ :: IO ()
kvServ = withSocketsDo $ do
    args <- getArgs
    let port = ((read . (!! 0)) args) :: Int
    socket <- listenOn $ PortNumber $fromIntegral port
    hSetBuffering stdout NoBuffering
    printf "Server auf Port %d gestartet." port
    store <- atomically $ newTVar $ fromList[(1,"test")]
    -- local management console
    forkIO $ manage socket
    -- local socket listener
    serve socket store

-- | connection handling, spawns new threads for each accept
serve :: Socket -> (TVar Store)-> IO()
serve socket store = handle exitSrv $ do
  (sock, host, _) <- accept socket
  forkIO $ dispatch sock store
  -- lokale bestätigung
  printf "Anfrage von %s beantwortet\n" host
  serve socket store

-- | protocol implementation
dispatch :: Handle -> (TVar Store) -> IO()
dispatch msg = do
    let cmd = take 3 msg
    case cmd of
        "add" -> return (add msg)-- hinzufuegen
        "rem" -> return (remove msg)-- loeschen
        "rqk" -> return (requestKey msg)-- key anfrage
        "rqv" -> return (requestVal msg)-- wert anfrage

-- | important stuff! do not touch!
manage :: Socket -> IO()
manage soc store = do
    line <- getLine
    case line of
        "exit" -> shutdown soc store
        _ -> manage soc store

-- | well ;)
shutdown soc store = do
    close soc
    -- saveStore store -- later

-- | highly trusted Exception
exitSrv :: SomeException -> IO()
exitSrv e = exitSuccess
