module Enigma where

import Control.Monad.State (State, evalState, get, put)
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
  { rotors :: [Rotor],
    reflector :: Reflector,
    plugboard :: Plugboard
  }

-- | Creates a new Enigma machine
-- | @param rotors An array of strings describing the rotor. The first 26
-- | characters is the mapping, or wiring. The 27th character must be a comma,
-- | followed by the notch(s). The 0th rotor is the fastest one, the last rotor
-- | is the closest to the reflector.
-- | @param rings The ring settings for each rotor.
-- | @param inits The initial setting for each rotor.
-- | @param reflector The reflector mapping, or wiring in a 26 character string.
-- | @param num_pairs The number of cables in the plugboard, i.e. the number of
-- | pairs of letters swapped.
-- | @param pairs A string with `2*num_pairs` characters describing how letters
-- | are swapped. Letter `pairs[i * 2]` is swapped with `pairs[i * 2 + 1]`, vice
-- | versa.
-- | @return An initialized machine.
newEnigma :: [String] -> [Int] -> [Int] -> String -> Int -> String -> Maybe Enigma
newEnigma str_rs rings inits str_refl np ps
  | length str_rs /= length rings || length rings /= length inits = Nothing
  | otherwise = do
      let arr = map (\(s, r, i) -> newRotor s r i) (zip3 str_rs rings inits)
      let refl = newReflector str_refl
      board <- newPlugboard np ps
      return (Enigma arr refl board)

tick :: Enigma -> Enigma
tick enigma@(Enigma {rotors = rs})
  | null rs = enigma
  | otherwise = enigma {rotors = tickRotors rs True}
  where
    tickRotors :: [Rotor] -> Bool -> [Rotor]
    tickRotors [] _ = []
    tickRotors [lastR] tickLast
      | tickLast = [tickRotor lastR]
      | otherwise = [lastR]
    tickRotors (r1 : r2 : _rs) tickLast =
      let tickSelf = isNotch r1 && not (noDoubleStep r2)
          r1Ticked = if tickLast || tickSelf then tickRotor r1 else r1
       in r1Ticked : tickRotors (r2 : _rs) tickSelf

tickEnigma :: State Enigma ()
tickEnigma = do
  enigma <- get
  let tickedEnigma = tick enigma
  put tickedEnigma

tickNEnigma :: Enigma -> Int -> Enigma
tickNEnigma enigma 0 = enigma
tickNEnigma enigma n = tickNEnigma (tick enigma) (n - 1)

encrypt :: Enigma -> String -> String
encrypt enigma msg = evalState (mapM encryptChar msg) enigma

encryptChar :: Char -> State Enigma Char
encryptChar char = do
  tickEnigma
  enigma <- get -- ticked enigma
  -- passing through the plugboard
  let afterPlugboard = connectPlugboard' (plugboard enigma) char

  -- passing forward through the rotors
  let forwardRotors = foldl (flip forwardConnectRotor) afterPlugboard (rotors enigma)

  -- reflection
  let reflected = connectReflector (reflector enigma) forwardRotors

  -- passing backward through the rotors
  let backwardRotors = foldr backwardConnectRotor reflected (rotors enigma)

  -- final pass through the plugboard
  let encodedChar = connectPlugboard' (plugboard enigma) backwardRotors
  return encodedChar

getSetting :: Enigma -> String
getSetting = map rotorSetting . rotors

instance Show Enigma where
  show = getSetting
