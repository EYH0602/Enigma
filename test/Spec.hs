import qualified TestEnigma as TE
import qualified TestPlugBoard as TP
import qualified TestReflector as TRe
import qualified TestRotor as TRo

main :: IO ()
main = do
  TE.testAll
  TP.testAll
  TRe.testAll
  TRo.testAll
