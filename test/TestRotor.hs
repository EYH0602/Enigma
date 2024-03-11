module TestRotor where

import Common
import Data.Char (chr, ord)
import Data.List (nub)
import Rotor
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    choose,
    elements,
    forAll,
    quickCheck,
    shuffle,
    (==>),
  )

instance Arbitrary Rotor where
  arbitrary = do
    rotorConfig <- elements [rotorI, rotorII, rotorIII, rotorIV, rotorV]
    return (newRotor rotorConfig 0 0)

-- Property to test if ticking the rotor updates its position correctly
prop_tickRotor :: Rotor -> Bool
prop_tickRotor rotor = pos (tickRotor rotor) == (pos rotor + 1) `mod` maxSize

-- Property to test ticking the rotor n times behaves as expected
prop_tickNRotor :: Rotor -> Int -> Bool
prop_tickNRotor rotor n =
  pos (tickNRotor rotor n) == (pos rotor + n) `mod` maxSize

-- Property to test if forward connection through a rotor maintains integrity
prop_forwardConnectIntegrity :: Rotor -> Char -> Property
prop_forwardConnectIntegrity rotor c =
  let validInput = ord 'A' <= ord c && ord c <= ord 'Z'
      output = forwardConnectRotor rotor c
      validOutput = ord 'A' <= ord output && ord output <= ord 'Z'
   in validInput ==> validOutput

-- Property to test if backward connection through a rotor is the inverse of forward connection
prop_backwardForwardInverse :: Rotor -> Char -> Property
prop_backwardForwardInverse rotor c =
  let validInput = ord 'A' <= ord c && ord c <= ord 'Z'
      forward = forwardConnectRotor rotor c
      backward = backwardConnectRotor rotor forward
   in validInput ==> (c == backward)

testAll :: IO ()
testAll = do
  quickCheck prop_tickRotor
  quickCheck prop_tickNRotor
  quickCheck prop_forwardConnectIntegrity
  quickCheck prop_backwardForwardInverse
