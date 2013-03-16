import System.Environment
import Data.HBDD.ROBDD
-- import Data.HBDD.ROBDDContext
-- import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations
import Control.Monad.Trans.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Data.HBDD.ROBDDDot
import Prelude hiding(and,or,not)
import Debug.Trace

main :: IO ()
main = do
       args <- getArgs
       let (bdd, context) = runState (doit $ read $ head args) mkContext -- FIXME: 1 reine
       putStrLn $ show $ getSat context bdd

traceIt :: (Show a) => a -> a
traceIt b = trace (show b) b

traceState :: (Show a) => (ROBDDState a) -> (ROBDDState a)
traceState robdd =
  mapState (\s@(a,b) -> traceShow b s) robdd

var :: Ord v => ROBDD v -> v
var (ROBDD _ v _ _) = v
var (ROBDDRef _ v _ _) = v
var _ = undefined

-- Checks if the cell (i,j) is forbidden if there is a queen in cell (x,y),
-- returns True is so.
inSight :: Int -> (Int,Int) -> ROBDDState (Int,Int)
inSight n (x,y) =
  do
  let invalid = (\(i,j) -> ((x,y) /= (i,j)) && ((x ==i) || (y==j) || (x+y)==(i+j) || (x-y) == (i-j)))
      cannot_exist = (foldl1 (.&.)
        (map (notC . singletonC) (filter invalid [(i,j) | i <- [1..n], j <- [1..n]])))
  (singletonC (x,y)) .=>. cannot_exist

doit :: Int -> ROBDDState (Int,Int)
doit n =
      do
       let queen_lst = foldl (\acc i ->
                 acc .&.
                 (foldl (\acc j -> (singletonC (i,j)) .|. acc) (return Zero) [1..n]))
               (return One) [1..n]
       -- not general
       foldl (.&.) queen_lst [inSight n (i,j) | i <- [1..n], j <- [1..n]]
