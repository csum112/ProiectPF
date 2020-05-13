module Main where
import Server

main :: IO ()
main = do
    putStrLn ("Starting TCP server on " ++ hostname ++ " port " ++ port)
    serverLoop hostname port
    where
        hostname = "127.0.0.1"
        port = "8989"
