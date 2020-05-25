module Server (serverLoop) where

import System.IO
import Network.Socket
import Cards
import Control.Concurrent
import Player
import Data.Tuple

serverLoop :: String -> String -> Int -> IO ()
serverLoop hostname port numberOfPlayers = do
    (addr:_) <- getAnyAddr hostname port
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (addrAddress addr)  -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    players <- gatherPlayers sock numberOfPlayers
    beginGame players


gatherPlayers :: Socket -> Int -> IO ([PlayerWrapper])
gatherPlayers _ 0 = do
    putStrLn "All players joined"
    return []
gatherPlayers sock numberOfPlayers = do
    conn <- accept sock     -- accept a connection and handle it
    m <- runConn conn            -- run our server's logic
    otherPlayers <- gatherPlayers sock (numberOfPlayers - 1)
    return ((PW m []):otherPlayers)

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


-- =========TODO: Finish implementing

beginGame :: [PlayerWrapper] -> IO ()
beginGame players = do
    putStrLn "Game has started. Now dealing cards"
    shufDeck <- shuffle deck
    game <- dealCardsGameWrapper players shufDeck numberOfCards
    gameLoop game 0
    where 
        deck = deckBuilder
        numberOfCards = 5

dealCardsGameWrapper :: [PlayerWrapper] -> [Card] -> Int -> IO(Game)
dealCardsGameWrapper players deck n = do 
    return (GM newplayers newdeck last_card) where
    (newplayers, newdeck) = dealCards players deck n
    last_card = (Card Spades Ace) -- Fix this

-- dealCards :: [PlayerWrapper] -> [Card] -> Int -> ([PlayerWrapper], [Card])
-- dealCards players _ deck 0 = (players, deck)
-- dealCards [] players deck n = (players, deck)
-- dealCards (hd:tl) playersbak deck n = ((player:tl), newNewDeck) where
--     ((_, newNewDeck), player) = ((dealCards tl playersbak newdeck n), newplayer) where
--             (newplayer, newdeck) = drawCard hd deck

dealCards :: [PlayerWrapper] -> [Card] -> Int -> ([PlayerWrapper], [Card])
dealCards players deck 0 = (players, deck)
dealCards players deck n = dealCards newplayers newdeck (n-1) where
    (newplayers, newdeck) = dealCardsRound players deck

dealCardsRound :: [PlayerWrapper] -> [Card] -> ([PlayerWrapper], [Card])
dealCardsRound [] deck = ([], deck)
dealCardsRound (hd: tl) deck = ((player:tl), newNewDeck) where
    ((_, newNewDeck), player) = ((dealCardsRound tl newdeck), newplayer) where
            (newplayer, newdeck) = drawCard hd deck

drawCard :: PlayerWrapper -> [Card] -> (PlayerWrapper, [Card])
drawCard (PW mvar hand) [] = ((PW mvar hand), [])
drawCard (PW mvar hand) (hd:tl) = ((PW mvar newhand), tl) where
    newhand = hd:hand


gameLoop :: Game -> Int -> IO()
gameLoop (GM players deck last_card) turn = do 
    putStrLn "Sending players the new game state"
    updatePlayers players last_card
    foo <- getLine
    gameLoop (GM players deck last_card) nextTurn where
        playerThisTurn = players !! turn
        nextTurn = (turn + 1) `mod` numberOfPlayers where
            numberOfPlayers = length players


updatePlayers :: [PlayerWrapper] -> Card -> IO()
updatePlayers [] _ = return ()
updatePlayers (hd:tl) last_card = do
    updatePlayer hd last_card
    updatePlayers tl last_card

updatePlayer :: PlayerWrapper -> Card -> IO()
updatePlayer (PW m hand) last_card = putMVar m (GameState hand last_card)