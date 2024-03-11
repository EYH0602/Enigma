module Rotor
  ( Rotor (..),
    newRotor,
    isNotch,
    noDoubleStep,
    tickRotor,
    tickNRotor,
    forwardConnectRotor,
    backwardConnectRotor,
    rotorSetting,
  )
where

import Common (maxSize)
import Data.Char (chr, ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Rotor = Rotor
  { mapping :: String,
    num_turnover :: Int,
    turnovers :: String,
    ring :: Int,
    pos :: Int
  }
  deriving (Eq)

splitRotor :: String -> (String, String)
splitRotor cs = (takeWhile (/= ',') cs, drop 1 (dropWhile (/= ',') cs))

newRotor :: String -> Int -> Int -> Rotor
newRotor rotor = Rotor mapping_str (length turnover_str) turnover_str
  where
    (mapping_str, turnover_str) = splitRotor rotor

posToLetter :: Int -> Char
posToLetter x = chr (ord 'A' + x)

isNotch :: Rotor -> Bool
isNotch rotor = posToLetter (pos rotor) `elem` turnovers rotor

noDoubleStep :: Rotor -> Bool
noDoubleStep rotor = num_turnover rotor == 0

tickRotor :: Rotor -> Rotor
tickRotor rotor = rotor {pos = (pos rotor + 1) `mod` maxSize}

tickNRotor :: Rotor -> Int -> Rotor
tickNRotor rotor n = rotor {pos = (pos rotor + n) `mod` maxSize}

forwardConnectRotor :: Rotor -> Char -> Char
forwardConnectRotor rotor c = chr (base + offsetOutput)
  where
    base = ord 'A'
    offsetInput = (ord c - base + pos rotor - ring rotor + maxSize) `mod` maxSize
    mappedChar = mapping rotor !! offsetInput
    offsetOutput = (ord mappedChar - base - pos rotor + ring rotor + maxSize) `mod` maxSize

backwardConnectRotor :: Rotor -> Char -> Char
backwardConnectRotor rotor c = chr (base + offsetOutput)
  where
    base = ord 'A'
    preMappedIndex = (ord c - base + pos rotor - ring rotor + maxSize) `mod` maxSize
    inverseMappedChar = fromJust $ elemIndex (chr (base + preMappedIndex)) (mapping rotor)
    offsetOutput = (inverseMappedChar - pos rotor + ring rotor + maxSize) `mod` maxSize

rotorSetting :: Rotor -> Char
rotorSetting rotor = chr (ord 'A' + pos rotor)

instance Show Rotor where
  show = return . rotorSetting
