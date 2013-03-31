import System.Environment
import Data.HBDD.ROBDD
import Data.HBDD.Operations
import Control.Monad.Trans.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Prelude hiding(and,or,not)

main :: IO ()
main = do
       args <- getArgs
       let (bdd, context) = runState (doit $ read $ head args) mkContext -- FIXME: 1 reine
       putStrLn $ show $ getSat context bdd

-- Checks if the cell (i,j) is forbidden if there is a queen in cell (x,y),
-- returns True is so.
inSight :: Int -> (Int,Int) -> ROBDDState (Int,Int)
inSight n (x,y) =
  do
  let invalid = (\(i,j) -> ((x,y) /= (i,j)) && ((x ==i) || (y==j) || (x+y)==(i+j) || (x-y) == (i-j)))
      cannot_exist = (foldr1 (.&.)
        (map (notC . singletonC) (filter invalid [(i,j) | i <- [1..n], j <- [1..n]])))
  (singletonC (x,y)) .=>. cannot_exist

doit :: Int -> ROBDDState (Int,Int)
doit n =
      do
       let queen_lst = foldr (\i acc ->
                 acc .&.
                 (foldr (\j acc1 -> (singletonC (i,j)) .|. acc1) (return Zero) [1..n]))
               (return One) [1..n]
       foldr (.&.) queen_lst [inSight n (i,j) | i <- [1..n], j <- [1..n]]
