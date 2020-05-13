module Server (serverLoop) where

import System.IO
import Network.Socket
import Cards

serverLoop :: String -> String -> IO ()
serverLoop hostname port = do
    (addr:_) <- getAnyAddr "127.0.0.1" "8787"
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (addrAddress addr)  -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock                              -- unimplemented


mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat

runConn :: (Socket, SockAddr) -> IO()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello! Try to say something"
    req <- hGetLine hdl
    hPutStrLn hdl ("You said: "++req)
    hPutStrLn hdl (renderCardList [(Card Spades Jack),(Card Hearts Queen),(Card Hearts Queen),(Card Hearts Queen),(Card Hearts Queen)])
    hClose hdl

getAnyAddr :: String -> String -> IO[AddrInfo]
getAnyAddr hostname port = getAddrInfo (Just defaultHints) (Just hostname) (Just port)



