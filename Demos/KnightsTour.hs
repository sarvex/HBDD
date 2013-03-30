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
import Data.List


main :: IO ()
main = do
       args <- getArgs
       let (bdd, context) = runState (doit $ read $ head args) mkContext
       putStrLn $ show $ getSat context bdd

var :: Ord v => ROBDD v -> v
var (ROBDD _ v _ _) = v
var (ROBDDRef _ v _ _) = v
var _ = undefined

type Graph = M.Map (Int,Int) (ROBDDState (Int,Int))

dtrace :: Show a => a -> a
dtrace elt = traceShow elt elt

getSucc :: Graph -> Int -> (Int,Int) -> [(Int,Int)]
getSucc graph size (x,y) =
      filter (\elt -> M.member elt graph)
      [(x + i,y + j) | i <- [-2,-1,1,2], j <- [-2,-1,1,2],
        (x + i) > 0 && (y + j) > 0 && (x + i) <= size && (y + j) <= size
        && (abs i) /= (abs j)]

path :: Int -> Int -> (Int,Int) -> Graph -> ROBDDState (Int,Int)
path 0 _ from graph = let (Just ret) = M.lookup from graph in ret
path 1 _ from graph = let (Just ret) = M.lookup from graph in ret
path n size from graph =
  do
  let (Just from') = M.lookup from graph
      graphNew     = M.delete from graph
      succLst      = getSucc graphNew size from
  if null succLst then
    return Zero
  else
    let ret = (map (\s -> path (n-1) size s graphNew) succLst) in
    from' .&. (foldl' (.|.) (head ret) (tail ret))


doit :: Int -> ROBDDState (Int,Int)
doit n =
  do
  let graph = M.fromList [((i,j),singletonC (i,j)) | i <- [1..n], j <- [1..n]]
  path (n*n) n (1,1) graph
