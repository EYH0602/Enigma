module Reflector where

import Data.Char (ord)

newtype Reflector = Reflector {mapping :: String} deriving (Show, Eq)

newReflector :: String -> Reflector
newReflector = Reflector

connectReflector :: Reflector -> Char -> Char
connectReflector r c = mapping r !! (ord c - ord 'A')
