module Player where
import Control.Concurrent
import System.IO
import Cards
import Data.Char

data PlayerWrapper = PW (MVar PlayerAction) [Card]

data PlayerAction = 
    DiscardTop [Card] | 
    DiscardBlind [Card] | 
    Claim | 
    ItsYourTurn | 
    HelpReq |
    GameState [Card] Card deriving Show

data Game = GM [PlayerWrapper] [Card] Card


cback :: Handle -> MVar PlayerAction-> IO ()
cback hdl m = do
    hPutStrLn hdl "Welcome! We are waiting for the other players to join"
    (GameState hand last_card) <- takeMVar m
    startPlaying hdl m hand last_card
    hClose hdl

startPlaying :: Handle -> MVar PlayerAction -> [Card] -> Card ->  IO()
startPlaying hdl m hand last_card = do
    hPutStrLn hdl "============================================"
    hPutStrLn hdl "The last played card is: "
    hPutStrLn hdl (show last_card)
    hPutStrLn hdl "and your hand is:"
    hPutStrLn hdl (renderCardList hand)
    (hand, last_card) <- speakToGM hdl m hand last_card
    startPlaying hdl m hand last_card


speakToGM :: Handle -> MVar PlayerAction -> [Card] -> Card-> IO ([Card], Card)
speakToGM hdl m hand last_card = do
    action <- takeMVar m
    tryToPlay hdl m hand last_card action

tryToPlay :: Handle -> MVar PlayerAction -> [Card] -> Card -> PlayerAction -> IO ([Card], Card)
tryToPlay _ _ _ _ (GameState hand last_card) = do return (hand, last_card)
tryToPlay hdl m hand last_card ItsYourTurn = do
    hPutStrLn hdl "Yay! Its your turn."
    usrInput <- hGetLine hdl
    putMVar m (tokensToPlayerAction (tokenizeString usrInput) hand)
    return (hand, last_card) 

tokenizeString :: String -> [String]
tokenizeString "" = []
tokenizeString str = (token:tokenizeString rest) where
    [(token, rest)] = lex str

tokensToPlayerAction :: [String] -> [Card] -> PlayerAction
tokensToPlayerAction ["claim"] _= Claim
tokensToPlayerAction ("discard":tl) hand = if length tl > 1 && length firstToken == 1 && (isDigit digit) then 
    futureAction else HelpReq where
        firstToken = tl !! 0
        digit = ((tl !! 0) !! 0)
        futureAction = case tokensToDiscardAction tl hand of 
            Nothing -> HelpReq
            Just action -> action
tokensToPlayerAction _ _ = HelpReq

tokensToDiscardAction :: [String] -> [Card] -> Maybe PlayerAction
tokensToDiscardAction ["top"] _ = Just (DiscardTop [])
tokensToDiscardAction ["blind"] _ = Just (DiscardBlind [])
tokensToDiscardAction (hd:tl) hand = if length hd == 1 && isDigit (hd!!0) then 
    tryParse else Nothing where 
        tryParse = ( case tokensToDiscardAction tl hand of 
            Just (DiscardBlind new_tl) -> if option < length hand then Just (DiscardBlind (card:new_tl)) else Nothing
            Just (DiscardTop new_tl) -> if option < length hand then Just (DiscardTop (card:new_tl)) else Nothing
            Nothing -> Nothing ) where 
                option = digitToInt (hd !! 0)
                card = hand!!option
tokensToDiscardAction _ _ = Nothing

