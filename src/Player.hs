module Player where
import Control.Concurrent
import System.IO
import Cards

data PlayerWrapper = PW (MVar PlayerAction) [Card]

data PlayerAction = 
    Discard [Card] | 
    Claim | 
    BlindPick |
    Pick Card |
    AnnounceTurn | 
    GameState [Card] Card

data Game = GM [PlayerWrapper] [Card] Card


cback :: Handle -> MVar PlayerAction-> IO ()
cback hdl m = do
    hPutStrLn hdl "Welcome! We are waiting for the other players to join"
    -- hPutStrLn hdl (renderCardList [(Card Spades Jack),(Card Hearts Queen),(Card Hearts Queen),(Card Hearts Queen),(Card Hearts Queen)])
    (GameState hand last_card) <- takeMVar m
    startPlaying hdl m hand last_card
    hClose hdl

startPlaying :: Handle -> MVar PlayerAction -> [Card] -> Card ->  IO()
startPlaying hdl m hand last_card = do
    hPutStrLn hdl (renderCardList hand)
    new_hand <- playRound hdl m hand
    startPlaying hdl m new_hand last_card


playRound :: Handle -> MVar PlayerAction -> [Card] -> IO [Card]
playRound hdl m hand = do 
    foo <- getLine
    return hand
