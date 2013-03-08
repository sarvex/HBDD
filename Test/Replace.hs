import Data.HBDD.ROBDD()
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations
import Prelude hiding(and,or,not)
import Data.HBDD.ROBDDDot

main :: IO ()
main =
  let (ac,a) = singleton mkContext "a"
      (not_ac,not_a) = not ac a
      (bc,b) = singleton not_ac "b"
      (cc,c) = singleton bc "c"
      (fc,f) = or cc c b
      (gc,g) = and fc not_a f
      --(hc,h) = replace gc not_a b g
  in do
    putStrLn $ show g
    putStrLn $ showDot gc g
