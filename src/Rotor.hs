module Rotor
  ( Rotor (..),
    newRotor,
    isNotch,
    noDoubleStep,
    tickRotor,
    tickNRotor,
    forwardConnectRotor,
    backwardConnectRotor,
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
  deriving (Show)

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
forwardConnectRotor self c = chr post_pos_char
  where
    iA = ord 'A'
    input = ord c - iA
    pre_mapping_idx = (maxSize + input + pos self - ring self) `mod` maxSize
    mapped_char = mapping self !! pre_mapping_idx
    post_pos_char = iA + (maxSize + ord mapped_char - iA - pos self + ring self) `mod` maxSize

backwardConnectRotor :: Rotor -> Char -> Char
backwardConnectRotor self c = chr output
  where
    ic = ord c
    iA = ord 'A'
    input = iA + (maxSize + ic - iA + pos self - ring self) `mod` maxSize
    i = fromJust $ elemIndex (chr input) (mapping self)
    output = iA + (maxSize - pos self + ring self + i) `mod` maxSize
