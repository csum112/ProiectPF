module Server (serverLoop) where

import System.IO
import Network.Socket
import Cards
import Control.Concurrent
import Player

serverLoop :: String -> String -> Int -> IO ()
serverLoop hostname port numberOfPlayers = do
    (addr:_) <- getAnyAddr hostname port
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (addrAddress addr)  -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    gatherPlayers sock numberOfPlayers
    gameLoop


gatherPlayers :: Socket -> Int -> IO ()
gatherPlayers _ 0 = putStrLn "All players joined"
gatherPlayers sock numberOfPlayers = do
    conn <- accept sock     -- accept a connection and handle it
    m <- runConn conn            -- run our server's logic
    gatherPlayers sock (numberOfPlayers - 1)

runConn :: (Socket, SockAddr) -> IO (MVar PlayerAction)
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    m <- newEmptyMVar
    tid <- forkIO (cback hdl m)   -- Moving client to a separate thread
    putStrLn ("Moved client to " ++ show tid)
    return m

getAnyAddr :: String -> String -> IO[AddrInfo]
getAnyAddr hostname port = getAddrInfo (Just defaultHints) (Just hostname) (Just port)

    


gameLoop :: IO ()
gameLoop = do
    putStrLn "Not implemented yet"

