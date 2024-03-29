module TestEnigma (testAll) where

import Common
import Data.Maybe (fromJust)
import Enigma
import Test.HUnit

enigma153 :: Enigma
enigma153 = fromJust $ newEnigma str_rotors rings inits iUkwA 5 "ECSONFIVTH"
  where
    str_rotors = [rotorI, rotorII, rotorIII]
    rings = [1, 5, 3]
    inits = [1, 5, 3]

testTick :: Test
testTick =
  TestCase
    ( do
        assertEqual "show enigma153 rotors" "BFD" (show enigma153)
        assertEqual "tick enigma153 once" "CFD" (show (tick enigma153))
        assertEqual "tick enigma153 153 times" "YLD" (show (tickNEnigma enigma153 153))
    )

testEnigmaOneRotorWORing :: Test
testEnigmaOneRotorWORing =
  TestCase
    ( do
        let rotors = [rotorI]
        let rings = [0]
        let inits = [1]
        let enigma = fromJust $ newEnigma rotors rings inits iUkwA 0 ""
        let plaintext = "C"
        let ciphertext = encryptEnigma enigma plaintext
        assertEqual "Encrypting 'C' with rotor I UKW B" "E" ciphertext
    )

testAll :: IO ()
testAll = do
  putStrLn "Testing Enigma ..."
  runTestTTAndExit $ TestList [testTick, testEnigmaOneRotorWORing]