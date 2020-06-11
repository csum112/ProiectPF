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
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 2
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
    return ((PW mOut mIn [] 0):otherPlayers)

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


beginGame :: [PlayerWrapper] -> IO ()
beginGame players = do
    putStrLn "Game has started. Now dealing cards"
    shufDeck <- shuffle deck
    game <- dealCardsGameWrapper players shufDeck numberOfCards
    (Over players) <- gameLoop game 0
    players <- return (map (\(PWR (PW m1 m2 _ score) _) -> (PW m1 m2 [] score)) players)
    putStrLn "Starting a new round"
    promptPlayersToStart players
    awaitPlayerPrompt players
    beginGame players
    where 
        deck = deckBuilder
        numberOfCards = 5


dealCardsGameWrapper :: [PlayerWrapper] -> [Card] -> Int -> IO(Game)
dealCardsGameWrapper players deck n = do 
    return (GM newplayers newdeck last_card adv) where
    (newplayers, newdeck, last_card, adv) = dealCards players deck n


dealCards :: [PlayerWrapper] -> [Card] -> Int -> ([PlayerWrapper], [Card], Card, Card)
dealCards players deck 0 = (players, new_deck, top_card, adv) where
    (top_card:(adv:new_deck)) = deck
dealCards players deck n = dealCards newplayers newdeck (n-1) where
    (newplayers, newdeck) = dealCardsRound players deck

dealCardsRound :: [PlayerWrapper] -> [Card] -> ([PlayerWrapper], [Card])
dealCardsRound [] deck = ([], deck)
dealCardsRound (hd: tl) deck = ((player:newTl), newNewDeck) where
    ((newTl, newNewDeck), player) = ((dealCardsRound tl newdeck), newplayer) where
            (newplayer, newdeck) = drawCard hd deck

drawCard :: PlayerWrapper -> [Card] -> (PlayerWrapper, [Card])
drawCard (PW mOut mIn hand score) [] = ((PW mOut mIn hand score), [])
drawCard (PW mOut mIn hand score) (hd:tl) = ((PW mOut mIn newhand score), tl) where
    newhand = hd:hand


gameLoop :: Game -> Int -> IO(Game)
gameLoop (Over players) _ = return (Over players) 
gameLoop (GM players deck last_card adv) turn = do 
    putStrLn "Sending players the new game state"
    updatePlayers players last_card adv
    game <- playerPlayTurn (GM players deck last_card adv) playerThisTurn turn
    gameLoop game nextTurn where
        playerThisTurn = players !! turn
        nextTurn = (turn + 1) `mod` numberOfPlayers where
            numberOfPlayers = length players


updatePlayers :: [PlayerWrapper] -> Card -> Card -> IO()
updatePlayers [] _ _ = return ()
updatePlayers (hd:tl) last_card adv = do
    updatePlayer hd last_card adv
    updatePlayers tl last_card adv

updatePlayer :: PlayerWrapper -> Card -> Card-> IO()
updatePlayer (PW mOut mIn hand score) last_card adv = putMVar mOut (GameState hand last_card adv)

playerPlayTurn :: Game -> PlayerWrapper -> Int -> IO(Game)
playerPlayTurn (Over players) _ _ = return (Over players)
playerPlayTurn game (PW mOut mIn hand score) turn = do
    putMVar mOut ItsYourTurn
    action <- takeMVar mIn
    makeGameChanges game (PW mOut mIn hand score) action turn

makeGameChanges :: Game -> PlayerWrapper -> PlayerAction -> Int -> IO(Game)
makeGameChanges (GM players _ _ adv) player Claim turn = calculateFinalScore players player adv
makeGameChanges game player HelpReq  turn = playerPlayTurn game player turn
makeGameChanges (GM players deck last_card adv) _ (DiscardTop toDiscard) turn = do
    return (GM (makePlayerChanges players turn toDiscard last_card) deck new_last_card adv) where 
        (new_last_card:_) = toDiscard
makeGameChanges (GM players (hd:tl) last_card adv) _ (DiscardBlind toDiscard) turn = do
    return (GM (makePlayerChanges players turn toDiscard hd) tl new_last_card adv) where 
        (new_last_card:_) = toDiscard

makePlayerChanges :: [PlayerWrapper] -> Int -> [Card] -> Card -> [PlayerWrapper]
makePlayerChanges (hd:tl) 0 toDiscard toAdd = ((changePlayer hd toDiscard toAdd):tl)
makePlayerChanges (hd:tl) n toDiscard toAdd =  (hd:(makePlayerChanges tl (n-1) toDiscard toAdd))


changePlayer :: PlayerWrapper -> [Card] -> Card -> PlayerWrapper
changePlayer (PW m1 m2 hand score) toDiscard toAdd = (PW m1 m2 newHand score) where
    newHand = (toAdd:withoutDiscarded) where
        withoutDiscarded = filter (\x -> not (x `elem` toDiscard)) hand


calculateFinalScore :: [PlayerWrapper] -> PlayerWrapper -> Card -> IO(Game)
calculateFinalScore players player adv = do
    newPlayers <- return (addRoundScore (setScoreWL (recCalcScore players adv) player))
    annoucePlayerScores newPlayers
    return (Over newPlayers)

annoucePlayerScores :: [PlayerWrapper] -> IO()
annoucePlayerScores [] = return ()
annoucePlayerScores ((PWR (PW mOut mIn _ score) roundScore):tl) = do
    putMVar mOut (AnnounceScore score roundScore)
    annoucePlayerScores tl


addRoundScore :: [PlayerWrapper] -> [PlayerWrapper]
addRoundScore [] = []
addRoundScore (hd:tl) = (newPlayer:rest) where 
    rest = addRoundScore tl
    newPlayer = (PWR innerPlayer roundScore) where
        (innerPlayer, roundScore) = ((PW m1 m2  hand (score + roundScore)), roundScore) where
            (PWR (PW m1 m2 hand score) roundScore) = hd

setScoreWL :: ([PlayerWrapper], Int) -> PlayerWrapper -> [PlayerWrapper]
setScoreWL ([], _) _ = []
setScoreWL ((hd:tl), minScore) theOneWhoCalledClaim = (newhd:rest) where 
    rest = setScoreWL (tl, minScore) theOneWhoCalledClaim
    newhd = if (currentPlayer == theOneWhoCalledClaim) && (playerScore /= minScore) then (PWR currentPlayer 50) else 
        if playerScore == minScore then (PWR currentPlayer 0) else (PWR currentPlayer playerScore) where 
            (PWR currentPlayer playerScore) = hd


recCalcScore :: [PlayerWrapper] -> Card -> ([PlayerWrapper], Int)
recCalcScore [] _ = ([], maxBound :: Int)
recCalcScore (hd:tl) adv = ((calculated:rest), currentMin) where
    (calculated, currentMin, rest) = ((PWR hd points), (min points previousMin), rest) where
        (rest, previousMin) = recCalcScore tl adv
        points = countPoints cards adv where 
            (PW _ _ cards _) = hd

countPoints :: [Card] -> Card -> Int
countPoints [] _ = 0
countPoints (hd:tl) adv = points + (countPoints tl adv) where 
    points = if hdCardValue == advCardValue then 0 else getCardPointValue hd where
        (Card _ hdCardValue) = hd
        (Card _ advCardValue) = adv


promptPlayersToStart :: [PlayerWrapper] -> IO ()
promptPlayersToStart [] = return ()
promptPlayersToStart ((PW mOut mIn _ _):tl) = do
    putMVar mOut GameStartPrompt
    promptPlayersToStart tl

awaitPlayerPrompt :: [PlayerWrapper] -> IO ()
awaitPlayerPrompt [] = return ()
awaitPlayerPrompt ((PW mOut mIn _ _):tl) = do
    action <- takeMVar mIn
    awaitPlayerPrompt tl














