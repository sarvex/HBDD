import Data.HBDD.ROBDD()
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations
import Prelude hiding(and,or,not)

main :: IO ()
main =
  let (ac,a) = singleton mkContext 'a'
      (bc,b) = singleton ac 'b'
      (cc, a_and_b) = and bc a b
      (dc, not_a) = not cc a
      (ec, not_b) = not dc b
      (fc, not_a_or_not_b) = or ec not_a not_b
      (_, not_a_or_b) = not fc not_a_or_not_b
  in do
      putStrLn $ show $ a_and_b
      putStrLn $ show $ not_a_or_b
      putStrLn $ show $ a_and_b == not_a_or_b

