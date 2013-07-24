import System.Environment (getArgs)
import System.IO (hFlush, hClose)
import Control.Monad (forever)
import Text.Printf (hPrintf, printf)
import Network (listenOn, accept, Socket, PortID (..))
import Control.Exception (handle, finally, SomeException)
import Control.Concurrent (forkIO)

main = getArgs >>= return . read . (!! 0)
               >>= listenOn . PortNumber . fromIntegral
               >>= forever . serve

serve socket = handle (\e -> print (e :: SomeException)) $ do
  (sock, host, _) <- accept socket

  forkIO $ flip finally (hClose sock) $ do
    text <- readFile "index.html"

    hPrintf sock "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s"
                 (length text) text >> hFlush sock

  printf "Anfrage von %s beantwortet\n" host

