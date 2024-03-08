module TestPlugBoard where

import Common (letters)
import Plugboard (Plugboard, connectPlugboard, newPlugboard)
import Test.QuickCheck
  ( Property,
    quickCheck,
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

testAll :: IO ()
testAll = do
  putStrLn "Testing Plugboard ..."
  quickCheck prop_SwapIdentity
  quickCheck prop_NoSwapIdentity
