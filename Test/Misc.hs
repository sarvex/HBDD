import Data.HBDD.ROBDD()
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations
import Prelude hiding(and,or)

main :: IO ()
main =
  let
    (ac,a) = singleton mkContext 1
    (bc,b) = singleton ac 2
    (cc,c) = singleton bc 3
    (dc, a_and_b) = and cc a b
    (ec, a_and_c) = and dc b c -- FIXME : wrong value
    (fc, a_and_b_or_a_and_c) = or ec a_and_b a_and_c
    (gc, b_or_c) = or fc b c
    (hc, a_and_b_or_c) = and gc a b_or_c
    in do
      putStrLn $ show $ a_and_b_or_a_and_c
      putStrLn $ show $ a_and_b_or_c
      putStrLn $ show $ a_and_b_or_a_and_c == a_and_b_or_c
