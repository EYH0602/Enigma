module TestEnigma where

import Plugboard (Plugboard, connectPlugboard, newPlugboard)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    OrderedList (..),
    Property,
    Testable (..),
    choose,
    classify,
    elements,
    forAll,
    frequency,
    label,
    oneof,
    quickCheck,
    sample,
    sized,
    withMaxSuccess,
    (==>),
  )

testAll :: IO ()
testAll = putStrLn "Test suite for Enigma not yet implemented"