module Player where
import Control.Concurrent
import System.IO
import Cards
import Data.Char

data PlayerWrapper = PW (MVar PlayerAction) (MVar PlayerAction) [Card] Int | PWR PlayerWrapper Int deriving Eq

data PlayerAction = 
    DiscardTop [Card] | 
    DiscardBlind [Card] | 
    Claim | 
    ItsYourTurn | 
    HelpReq |
    GameState [Card] Card Card | 
    AnnounceScore Int Int

data Game = GM [PlayerWrapper] [Card] Card Card | Over [PlayerWrapper]


cback :: Handle -> (MVar PlayerAction) -> (MVar PlayerAction)-> IO ()
cback hdl mOut mIn = do
    hPutStrLn hdl "Welcome! We are waiting for the other players to join"
    (GameState hand last_card adv) <- takeMVar mOut
    startPlaying hdl mOut mIn hand last_card adv
    hClose hdl

startPlaying :: Handle -> MVar PlayerAction ->  MVar PlayerAction -> [Card] -> Card -> Card ->  IO()
startPlaying hdl mOut mIn hand last_card adv = do
    hPutStrLn hdl "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    hPutStrLn hdl (renderDownCards last_card adv)
    hPutStrLn hdl (renderCardList hand)
    (hand, last_card, adv) <- speakToGM hdl mOut mIn hand last_card adv
    startPlaying hdl mOut mIn hand last_card adv


speakToGM :: Handle -> MVar PlayerAction ->  MVar PlayerAction -> [Card] -> Card -> Card -> IO ([Card], Card, Card)
speakToGM hdl mOut mIn hand last_card adv = do
    action <- takeMVar mOut
    tryToPlay hdl mOut mIn hand last_card adv action

tryToPlay :: Handle -> MVar PlayerAction ->  MVar PlayerAction -> [Card] -> Card -> Card-> PlayerAction -> IO ([Card], Card, Card)
tryToPlay _ _ _ _ _ _ (GameState hand last_card adv) = do return (hand, last_card, adv)
tryToPlay hdl mOut mIn hand last_card adv ItsYourTurn = do
    hPutStrLn hdl "Yay! Its your turn."
    usrInput <- hGetLine hdl
    putMVar mIn (tokensToPlayerAction (tokenizeString usrInput) hand)
    return (hand, last_card, adv) 
tryToPlay hdl mOut mIn hand last_card adv (AnnounceScore score roundScore) = do
    hPutStrLn hdl ("You've got " ++ (show roundScore) ++ " points this round.")
    hPutStrLn hdl ("You now have " ++ (show score) ++ " points.")
    speakToGM hdl mOut mIn hand last_card adv

tokenizeString :: String -> [String]
tokenizeString "" = []
tokenizeString str = (token:tokenizeString rest) where
    [(token, rest)] = lex str

tokensToPlayerAction :: [String] -> [Card] -> PlayerAction
tokensToPlayerAction ["claim", ""] _= Claim
tokensToPlayerAction ("discard":tl) hand = if length tl > 1 && length firstToken == 1 && (isDigit digit) then 
    futureAction else HelpReq where
        firstToken = tl !! 0
        digit = ((tl !! 0) !! 0)
        futureAction = case tokensToDiscardAction tl hand of 
            Nothing -> HelpReq
            Just action -> action
tokensToPlayerAction _ _ = HelpReq

tokensToDiscardAction :: [String] -> [Card] -> Maybe PlayerAction
tokensToDiscardAction ["top", ""] _ = Just (DiscardTop [])
tokensToDiscardAction ["blind", ""] _ = Just (DiscardBlind [])
tokensToDiscardAction (hd:tl) hand = if length hd == 1 && isDigit (hd!!0) then 
    tryParse else Nothing where 
        tryParse = ( case tokensToDiscardAction tl hand of 
            Just (DiscardBlind new_tl) -> if option < length hand then Just (DiscardBlind (card:new_tl)) else Nothing
            Just (DiscardTop new_tl) -> if option < length hand then Just (DiscardTop (card:new_tl)) else Nothing
            Nothing -> Nothing ) where 
                option = digitToInt (hd !! 0)
                card = hand!!option
tokensToDiscardAction _ _ = Nothing




