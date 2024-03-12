module Common where

-- Max number for many buffers
maxSize :: Int
maxSize = 26

-- Letters for reference
letters :: String
letters = ['A' .. 'Z']

-- Rotor I ~ V used in Enigma I
rotorI :: String
rotorI = "EKMFLGDQVZNTOWYHXUSPAIBRCJ,Q"

rotorII :: String
rotorII = "AJDKSIRUXBLHWTMCQGZNPYFVOE,E"

rotorIII :: String
rotorIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO,V"

rotorIV :: String
rotorIV = "ESOVPZJAYQUIRHXLNFTGKDCMWB,J"

rotorV :: String
rotorV = "VZBRGITYUPSDNHLXAWMJQOFECK,Z"

-- Reflectors used in Enigma I
iUkwA :: String
iUkwA = "EJMZALYXVBWFCRQUONTSPIKHGD"

iUkwB :: String
iUkwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

iUkwC :: String
iUkwC = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

-- Rotors used in Enigma M4
rotorVI :: String
rotorVI = "JPGVOUMFYQBENHZRDKASXLICTW,MZ"

rotorVII :: String
rotorVII = "NZJHGRCXMYSWBOUFAIVLPEKQDT,MZ"

rotorVIII :: String
rotorVIII = "FKQHTLXOCBJSPDZRAMEWNIUYGV,MZ"

rotorBETA :: String
rotorBETA = "LEYJVCNIXWPBQMDRTAKZGFUHOS,\0"

rotorGAMMA :: String
rotorGAMMA = "FSOKANUERHMBTIYCWLQPZXVGJD,\0"

-- Reflectors in Enigma M4
ukwB :: String
ukwB = "ENKQAUYWJICOPBLMDXZVFTHRGS"

ukwC :: String
ukwC = "RDOBJNTKVEHMLFCWZAXGYIPSUQ"

