module Plugboard (Plugboard, newPlugboard, connectPlugboard, connectPlugboard') where

import Common (letters)
import Data.Char (ord)

type CharMapping = [(Char, Char)]

newtype Plugboard = Plugboard {mapping :: String} deriving (Show)

makeMapping :: String -> CharMapping
makeMapping [] = []
makeMapping (x : y : xs) = (x, y) : makeMapping xs
makeMapping _ = error "Invalid pair list"

newPlugboard :: Int -> String -> Maybe Plugboard
newPlugboard numPairs pairs =
  if length pairs /= numPairs * 2
    then Nothing
    else Just . Plugboard $ foldr swapLetters letters (makeMapping pairs)

swapLetters :: (Char, Char) -> String -> String
swapLetters (x, y) = map (\c -> if c == x then y else if c == y then x else c)

connectPlugboard' :: Plugboard -> Char -> Char
connectPlugboard' pb c = mapping pb !! (ord c - ord 'A')

connectPlugboard :: Maybe Plugboard -> Char -> Maybe Char
connectPlugboard board c = board >>= Just . flip connectPlugboard' c
