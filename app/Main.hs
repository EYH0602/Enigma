{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import Common
import Data.Maybe (fromJust)
import Enigma (Enigma, encryptEnigma, newEnigma)
import System.Console.CmdArgs

enigma153 :: Enigma
enigma153 = fromJust $ newEnigma str_rotors rings inits iUkwA 5 "ECSONFIVTH"
  where
    str_rotors = [rotorI, rotorII, rotorIII]
    rings = [1, 5, 3]
    inits = [1, 5, 3]

encrypt :: String -> String
encrypt = encryptEnigma enigma153

newtype Plaintext = Plaintext {message :: String}
  deriving (Show, Data, Typeable)

text :: Plaintext
text =
  Plaintext {message = def &= help "The message to encrypt."}
    &= verbosity
    &= summary "Enigma Machine v0.0.0"

main :: IO ()
main = do
  cli_args <- cmdArgs text
  putStrLn $ encrypt (message cli_args)