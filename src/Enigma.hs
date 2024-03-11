module Enigma where

import Common
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Plugboard (Plugboard, connectPlugboard', newPlugboard)
import Reflector (Reflector, connectReflector, newReflector)
import Rotor
  ( Rotor (..),
    backwardConnectRotor,
    forwardConnectRotor,
    isNotch,
    newRotor,
    noDoubleStep,
    rotorSetting,
    tickRotor,
  )

data Enigma = Enigma
  { num_rotors :: Int,
    rotors :: [Rotor],
    reflector :: Reflector,
    plugboard :: Plugboard
  }
  deriving (Eq)

-- | Creates a new Enigma machine
-- | @param num_rotors The number of rotors used in this machine
-- | @param rotors An array of strings describing the rotor. The first 26
-- | charactors is the mapping, or wiring. The 27th charactor must be a comma,
-- | followed by the notch(s). The 0th rotor is the fastest one, the last rotor
-- | is the closest to the reflector.
-- | @param rings The ring settings for each rotor. It is guaranteed to have a
-- | length of `num_rotors`.
-- | @param inits The initial setting for each rotor. It is guaranteed to have a
-- | length of `num_rotors`.
-- | @param reflector The reflector mapping, or wiring in a 26 charactor string.
-- | @param num_pairs The number of cables in the plugboard, i.e. the number of
-- | pairs of letters swapped.
-- | @param pairs A string with `2*num_pairs` characters describing how letters
-- | are swapped. Letter `pairs[i * 2]` is swapped with `pairs[i * 2 + 1]`, vice
-- | versa.
-- | @return An initialized machine.
newEnigma :: Int -> [String] -> [Int] -> [Int] -> String -> Int -> String -> Maybe Enigma
newEnigma nr str_rs rings inits str_refl np ps = do
  let arr = map (\(s, r, i) -> newRotor s r i) (zip3 str_rs rings inits)
  let refl = newReflector str_refl
  board <- newPlugboard np ps
  return (Enigma nr arr refl board)

tickEnigma :: Enigma -> Enigma
tickEnigma enigma@(Enigma {num_rotors = 0}) = enigma
tickEnigma enigma@(Enigma {rotors = rs}) = enigma {rotors = tickRotors rs True}
  where
    tickRotors :: [Rotor] -> Bool -> [Rotor]
    tickRotors [] _ = []
    tickRotors [lastR] tickLast
      | tickLast = [tickRotor lastR]
      | otherwise = [lastR]
    tickRotors (r1 : r2 : rs) tickLast =
      let tickSelf = isNotch r1 && not (noDoubleStep r2)
          r1Ticked = if tickLast || tickSelf then tickRotor r1 else r1
       in r1Ticked : tickRotors (r2 : rs) tickSelf

tickNEnigma :: Enigma -> Int -> Enigma
tickNEnigma enigma 0 = enigma
tickNEnigma enigma n = tickNEnigma (tickEnigma enigma) (n - 1)

encryptEnigma :: Enigma -> String -> String
encryptEnigma _ [] = []
encryptEnigma initEnigma (c : cs) =
  let (tickedEnigma, encryptedChar) = encryptChar initEnigma c
   in encryptedChar : encryptEnigma tickedEnigma cs

encryptChar :: Enigma -> Char -> (Enigma, Char)
encryptChar e ' ' = (e, ' ')
encryptChar enigma char = (tickedEnigma, finalOutput)
  where
    -- initial tick for each character to be encrypted
    tickedEnigma = tickEnigma enigma

    -- passing through the plugboard
    afterPlugboard = connectPlugboard' (plugboard tickedEnigma) char

    -- passing forward through the rotors
    forwardRotors = foldl (flip forwardConnectRotor) afterPlugboard (rotors tickedEnigma)

    -- reflection
    reflected = connectReflector (reflector tickedEnigma) forwardRotors

    -- passing backward through the rotors
    backwardRotors = foldr backwardConnectRotor reflected (rotors tickedEnigma)

    -- final pass through the plugboard
    finalOutput = connectPlugboard' (plugboard tickedEnigma) backwardRotors

getSetting :: Enigma -> String
getSetting = map rotorSetting . rotors

instance Show Enigma where
  show = getSetting
