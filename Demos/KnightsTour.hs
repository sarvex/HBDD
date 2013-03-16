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
import qualified Data.Map.Strict as M
import Data.Maybe


main :: IO ()
main = do
       args <- getArgs
       let (bdd, context) = runState (doit $ read $ head args) mkContext
       putStrLn $ show $ getSat context bdd

var :: Ord v => ROBDD v -> v
var (ROBDD _ v _ _) = v
var (ROBDDRef _ v _ _) = v
var _ = undefined

type Graph = M.Map (Int,Int,Int) (ROBDDState (Int,Int,Int))

getSucc :: Graph -> Int -> (Int,Int,Int) -> [ROBDDState (Int,Int,Int)]

getSucc graph size (x,y,t) =
  do
    catMaybes $ map (\k -> M.lookup k graph)
      [(x + i,y + j,t + 1) | i <- [-2,-1,1,2], j <- [-2,-1,1,2],
        (x + i) > 0 && (y + j) > 0 && (x + i) < size && (y + j) < size
        && (abs i) /= (abs j), t < size*size]

remove_futures :: Int -> (Int,Int,Int) -> Graph -> Graph
remove_futures time (x,y,t) graph =
  foldl (\g cur ->let coor = (x,y,cur) in M.delete coor g) graph [t..time]

path :: Int -> Int -> ROBDDState (Int,Int,Int) -> Graph -> ROBDDState (Int,Int,Int)
path 0 _ from _ = from
path n size from graph =
  do
    from' <- from
    foldl (\acc elt ->
      do
        elt' <- elt
        acc .|.  (from .&. (path (n-1) size elt (remove_futures (n*n) (var from') graph))))
        (return Zero) (getSucc graph size $ var from')


doit :: Int -> ROBDDState (Int,Int,Int)
doit n =
  do
  let graph = M.fromList [((i,j,t),singletonC (i,j,t)) | i <- [1..n], j <- [1..n], t <- [1..n*n]]
  M.foldl (\acc elt ->
    do
      elt' <- elt
      acc .|. (path (n-1) n elt (M.delete (var elt') graph))) (return Zero) graph
