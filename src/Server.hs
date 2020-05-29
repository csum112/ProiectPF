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
    (mOut, mIn) <- runConn conn            -- run our server's logic
    otherPlayers <- gatherPlayers sock (numberOfPlayers - 1)
    return ((PW mOut mIn []):otherPlayers)

runConn :: (Socket, SockAddr) -> IO (MVar PlayerAction, MVar PlayerAction)
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    mOut <- newEmptyMVar
    mIn <- newEmptyMVar
    tid <- forkIO (cback hdl mOut mIn)   -- Moving client to a separate thread
    putStrLn ("Moved client to " ++ show tid)
    return (mOut, mIn)

getAnyAddr :: String -> String -> IO[AddrInfo]
getAnyAddr hostname port = getAddrInfo (Just defaultHints) (Just hostname) (Just port)


-- =========TODO: Finish implementing

beginGame :: [PlayerWrapper] -> IO ()
beginGame players = do
    putStrLn "Game has started. Now dealing cards"
    shufDeck <- shuffle deck
    game <- dealCardsGameWrapper players shufDeck numberOfCards
    round <- gameLoop game 0
    return ()
    where 
        deck = deckBuilder
        numberOfCards = 5

dealCardsGameWrapper :: [PlayerWrapper] -> [Card] -> Int -> IO(Game)
dealCardsGameWrapper players deck n = do 
    return (GM newplayers newdeck last_card) where
    (newplayers, newdeck, last_card) = dealCards players deck n




dealCards :: [PlayerWrapper] -> [Card] -> Int -> ([PlayerWrapper], [Card], Card)
dealCards players deck 0 = (players, new_deck, top_card) where
    (top_card:new_deck) = deck
dealCards players deck n = dealCards newplayers newdeck (n-1) where
    (newplayers, newdeck) = dealCardsRound players deck

dealCardsRound :: [PlayerWrapper] -> [Card] -> ([PlayerWrapper], [Card])
dealCardsRound [] deck = ([], deck)
dealCardsRound (hd: tl) deck = ((player:newTl), newNewDeck) where
    ((newTl, newNewDeck), player) = ((dealCardsRound tl newdeck), newplayer) where
            (newplayer, newdeck) = drawCard hd deck

drawCard :: PlayerWrapper -> [Card] -> (PlayerWrapper, [Card])
drawCard (PW mOut mIn hand) [] = ((PW mOut mIn hand), [])
drawCard (PW mOut mIn hand) (hd:tl) = ((PW mOut mIn newhand), tl) where
    newhand = hd:hand


gameLoop :: Game -> Int -> IO(Game)
gameLoop (Over players) _ = return (Over players) 
gameLoop (GM players deck last_card) turn = do 
    putStrLn "Sending players the new game state"
    updatePlayers players last_card
    game <- playerPlayTurn (GM players deck last_card) playerThisTurn turn
    gameLoop game nextTurn where
        playerThisTurn = players !! turn
        nextTurn = (turn + 1) `mod` numberOfPlayers where
            numberOfPlayers = length players


updatePlayers :: [PlayerWrapper] -> Card -> IO()
updatePlayers [] _ = return ()
updatePlayers (hd:tl) last_card = do
    updatePlayer hd last_card
    updatePlayers tl last_card

updatePlayer :: PlayerWrapper -> Card -> IO()
updatePlayer (PW mOut mIn hand) last_card = putMVar mOut (GameState hand last_card)

playerPlayTurn :: Game -> PlayerWrapper -> Int -> IO(Game)
playerPlayTurn (Over players) _ _ = return (Over players)
playerPlayTurn game (PW mOut mIn hand) turn = do
    putMVar mOut ItsYourTurn
    action <- takeMVar mIn
    makeGameChanges game (PW mOut mIn hand) action turn

makeGameChanges :: Game -> PlayerWrapper -> PlayerAction -> Int -> IO(Game)
makeGameChanges (GM players _ _) player Claim turn = calculateFinalScore players player --IMPLEMENT
makeGameChanges game player HelpReq  turn = playerPlayTurn game player turn
makeGameChanges (GM players deck last_card) _ (DiscardTop toDiscard) turn = do
    return (GM (makePlayerChanges players turn toDiscard last_card) deck new_last_card) where 
        (new_last_card:_) = toDiscard
makeGameChanges (GM players (hd:tl) last_card) _ (DiscardBlind toDiscard) turn = do
    return (GM (makePlayerChanges players turn toDiscard hd) tl new_last_card) where 
        (new_last_card:_) = toDiscard

makePlayerChanges :: [PlayerWrapper] -> Int -> [Card] -> Card -> [PlayerWrapper]
makePlayerChanges (hd:tl) 0 toDiscard toAdd = ((changePlayer hd toDiscard toAdd):tl)
makePlayerChanges (hd:tl) n toDiscard toAdd =  (hd:(makePlayerChanges tl (n-1) toDiscard toAdd))


changePlayer :: PlayerWrapper -> [Card] -> Card -> PlayerWrapper
changePlayer (PW m1 m2 hand) toDiscard toAdd = (PW m1 m2 newHand) where
    newHand = (toAdd:withoutDiscarded) where
        withoutDiscarded = filter (\x -> not (x `elem` toDiscard)) hand


calculateFinalScore :: [PlayerWrapper] -> PlayerWrapper -> IO(Game)
calculateFinalScore players player = return (Over newPlayers) where 
    newPlayers = recCalcScore players

recCalcScore :: [PlayerWrapper] ->[PlayerWrapper]
recCalcScore [] = []
recCalcScore (hd:tl) = (calculated:(recCalcScore tl)) where 
    calculated = (PWS m1 m2 points) where 
        m1 = m1
        m2 = m2
        points = countPoints cards where 
            (PW m1 m2 cards) = hd

countPoints :: [Card] -> Int
countPoints (hd:tl) = points + (countPoints tl) where 
    points = getCardPointValue hd