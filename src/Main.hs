module Main where
import Server

main :: IO ()
main = do
    putStrLn ("Starting TCP server on " ++ hostname ++ " port " ++ port)
    serverLoop hostname port numberOfPlayers
    where
        hostname = "0.0.0.0"
        port = "333"
        numberOfPlayers = 2

