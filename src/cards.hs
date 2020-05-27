module Cards where

import System.Random
import Data.Array.IO
import Control.Monad


data CardValue = Ace | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | Jack | Queen | King deriving (Enum, Eq)

data CardKind = Clubs | Spades | Diamonds | Hearts deriving (Enum, Eq)

data Card = Card CardKind CardValue deriving Eq

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

instance Show CardKind where
    show Spades = "\xe2\x99\xa0"
    show Clubs = "\xe2\x99\xa3"
    show Hearts = "\xe2\x9d\xa4"
    show Diamonds = "\xe2\x97\x86"

instance Show CardValue where
    show Ace = "A"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show V2 = "2"
    show V3 = "3"
    show V4 = "4"
    show V5 = "5"
    show V6 = "6"
    show V7 = "7"
    show V8 = "8"
    show V9 = "9"
    show V10 = "10"

instance Show Card where
    show (Card kind value) = " _____ \n|     |\n| " ++ (show kind) ++ " " ++ (show value) ++ whitespace ++ "|\n|     |\n|_____|\n" where
        whitespace = if (value == V10) then "" else " "


splitLeft :: String -> String
splitLeft "" = ""
splitLeft (hd:tl) = if hd == '\n' then "" else (hd:rest) where
    rest = splitLeft tl

splitRight :: String -> String
splitRight "" = ""
splitRight (hd:tl) = if hd == '\n' then tl else (splitRight tl)

splitOnNewLine :: String -> [String]
splitOnNewLine "" = []
splitOnNewLine blockOfText = (head:tail) where
    head = splitLeft blockOfText
    tail = splitOnNewLine (splitRight blockOfText)

interpolateStrings :: [String] -> [String] -> [String]
interpolateStrings [] [] = []
interpolateStrings (h1:t1) (h2:t2) = hf:tf where
    hf = h1 ++ "  " ++ h2
    tf = interpolateStrings t1 t2


joinStringRows :: [String] -> String
joinStringRows [] = ""
joinStringRows (hd:tl) = hd ++ "\n" ++ (joinStringRows tl)

join2cards :: Card -> Card -> String
join2cards c1 c2 = joinStringRows (interpolateStrings l1 l2) where
    l1 = splitOnNewLine (show c1)
    l2 = splitOnNewLine (show c2)

reduceStrings :: [[String]] -> [String]
reduceStrings [] = []
reduceStrings (hd:tl) = if (length tl) > 0 
    then interpolateStrings hd (reduceStrings tl)
    else hd

cardsToStrings :: [Card] -> Int -> [[String]]
cardsToStrings [] _ = []
cardsToStrings (hd:tl) index = (strs : (cardsToStrings tl (index + 1))) where
    strs = splitOnNewLine (show hd) ++ ["  (" ++ (show index) ++ ")  "]

renderCardList :: [Card] -> String
renderCardList cards = joinStringRows (reduceStrings cardStrings) where
    cardStrings = cardsToStrings cards 0





