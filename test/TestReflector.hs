module TestReflector where

import Common (iUkwA, letters)
import Data.Char (ord)
import Reflector (connectReflector, newReflector)
import Test.QuickCheck
  ( Property,
    quickCheck,
    (==>),
  )

prop_iUkwA :: Char -> Property
prop_iUkwA c =
  (c `elem` letters)
    ==> let reflector = newReflector iUkwA
         in connectReflector reflector c == iUkwA !! (ord c - ord 'A')

testAll :: IO ()
testAll = do
  putStrLn "Testing Reflector ..."
  quickCheck prop_iUkwA
