import Data.HBDD.ROBDD()
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations
import Prelude hiding(and,or,not)
import Data.HBDD.ROBDDDot

main :: IO ()
main =
  let (ac,a) = singleton mkContext (1::Int)
      (not_ac,_) = not ac a
      (bc,b) = singleton not_ac 2
      (cc,c) = singleton bc 3
      (fc,f) = or cc c b
      (_,g) = and fc c f
      (retC,ret) = exists fc a g
  in do
    putStrLn $ show $ ret
    putStrLn $ showDot retC ret
