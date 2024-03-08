module Rotor (alphabet) where

alphabet :: [Char]
alphabet = ['A' .. 'Z']

data Rotor = Rotor {_out :: String, _in :: String, _step :: Char} deriving (Show, Eq)

type Rotors = (Rotor, Rotor, Rotor) -- each Enigma machine has a set of three Rotors
