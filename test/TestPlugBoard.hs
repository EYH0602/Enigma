module TestPlugBoard where

import Common (letters)
import Data.List (nub, sort)
import Plugboard (Plugboard, connectPlugboard, connectPlugboard', newPlugboard)
import Test.QuickCheck
  ( Gen,
    Property,
    forAll,
    quickCheck,
    shuffle,
    (==>),
  )

-- swapping a pair and then applying the plugboard to one of them returns the other
prop_SwapIdentity :: Char -> Char -> Property
prop_SwapIdentity x y =
  (x /= y && x `elem` letters && y `elem` letters)
    ==> let pb = newPlugboard 1 [x, y]
         in connectPlugboard pb x == Just y && connectPlugboard pb y == Just x

-- applying connectPlugboard on a letter without any swaps returns the letter itself
prop_NoSwapIdentity :: Char -> Property
prop_NoSwapIdentity c =
  c `elem` letters
    ==> connectPlugboard (newPlugboard 0 "") c == Just c

makePairs :: String -> [(Char, Char)]
makePairs zs = zip xs ys
  where
    (xs, ys) = splitAt ((length zs + 1) `div` 2) zs

-- generator for Plugboard configurations
genPlugboard :: Gen (Maybe Plugboard)
genPlugboard = do
  shuffled <- shuffle letters
  let pairs = take 20 $ makePairs shuffled -- Take first 10 pairs for example
      pairString = concatMap (\(a, b) -> [a, b]) pairs
  return $ newPlugboard (length pairs `div` 2) pairString

-- checker for randomly generated Plugboard
isValidPlugboard :: Maybe Plugboard -> Bool
isValidPlugboard (Just pb) = all (\c -> connectPlugboard' pb c == c || connectPlugboard' pb (connectPlugboard' pb c) == c) letters
isValidPlugboard Nothing = True

prop_connectPlugboard :: Property
prop_connectPlugboard = forAll genPlugboard isValidPlugboard

testAll :: IO ()
testAll = do
  putStrLn "Testing Plugboard ..."
  quickCheck prop_SwapIdentity
  quickCheck prop_NoSwapIdentity
  quickCheck prop_connectPlugboard
