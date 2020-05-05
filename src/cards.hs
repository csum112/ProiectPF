import System.Random
import Data.Array.IO
import Control.Monad


data CardValue = Ace | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | Jack | Queen | King deriving (Show, Enum, Eq)

data CardKind = Clubs | Spades | Diamonds | Hearts deriving (Show, Enum, Eq)

data Card = Card CardKind CardValue deriving Show

deckBuilderHelper :: [CardKind] -> [CardValue] -> [Card]
deckBuilderHelper [] _ = []
deckBuilderHelper (hd:tl) [] = deckBuilderHelper tl (enumFrom Ace)
deckBuilderHelper (khd:ktl) (vhd:vtl) = (Card khd vhd):(deckBuilderHelper (khd:ktl) vtl)


deckBuilder :: [Card]
deckBuilder = deckBuilderHelper kinds values where
    kinds = enumFrom Clubs
    values = enumFrom Ace


-- Taken from https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


