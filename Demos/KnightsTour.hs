{-# LANGUAGE DoAndIfThenElse #-}
import System.Environment
import Data.HBDD.ROBDD
import Data.HBDD.Operations()
import Control.Monad.Trans.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Prelude hiding(and,or,not)
import Data.List

main :: IO ()
main = do
       args <- getArgs
       let (bdd, _) = runState (doit $ read $ head args) mkContext
       case bdd of
         One -> putStrLn "All the cells can be visited"
         _ -> putStrLn "One or more cells cannot be visited"

getSucc :: Int -> (Int,Int) -> [(Int,Int)]
getSucc n (x,y) =
      [(x + i,y + j) | i <- [-2,-1,1,2], j <- [-2,-1,1,2],
        (x + i) > 0 && (y + j) > 0 && (x + i) <= n && (y + j) <= n
        && (abs i) /= (abs j)]

path :: Int -> ROBDDState (Int,Int) -> [(Int,Int)] -> ROBDDState (Int,Int)
path n ref nodes =
  do
  ref' <- ref
  let succLst = nub $ concat $ map (getSucc n) nodes
      result  = ref .&. (foldl1 (.&.) $ map singletonC succLst)
  result' <- result
  if result' == ref' then
    result
  else
    path n result succLst


doit :: Int -> ROBDDState (Int,Int)
doit n =
  do
  (foldl1 (.&.) [singletonC (i,j) | i <- [1..n], j <- [1..n]]) .<=>. (path n (singletonC (1,1)) [(1,1)])
