import Network
import Control.Concurrent
import System.IO
 
func = do
  x <- Just 3
  y <- Just 4
  return $ x + y




main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       hPutStr h msg
       hFlush h
       hClose h
 
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"