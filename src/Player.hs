module Player where
import Control.Concurrent
import System.IO
import Cards

data PlayerWrapper = PW (MVar PlayerAction)

data PlayerAction = Discard [Card] | Claim


cback :: Handle -> MVar PlayerAction-> IO ()
cback hdl m = do
    hPutStrLn hdl "Hello! Try to say something"
    hPutStrLn hdl (renderCardList [(Card Spades Jack),(Card Hearts Queen),(Card Hearts Queen),(Card Hearts Queen),(Card Hearts Queen)])
    hPutStrLn hdl ("Some blocking action")
    req <- hGetLine hdl
    hClose hdl