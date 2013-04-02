{-# LANGUAGE DoAndIfThenElse #-}
import System.Environment
import Data.HBDD.ROBDD
import Data.HBDD.Operations
import Control.Monad.Trans.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Prelude hiding(and,or,not)
import Debug.Trace

main :: IO ()
main = do
       args <- getArgs
       let (bdd, context) = runState (doit $ read $ head args) mkContext
       putStrLn $ show $ getSatList context bdd
       -- case getSat context bdd of
       --   Just _ -> putStrLn "Satisfiable"
       --   _ -> putStrLn "No solutions"

getSucc :: Int -> (Int,Int) -> [(Int,Int)]
getSucc size (x,y) =
      [(x + i,y + j) | i <- [-2,-1,1,2], j <- [-2,-1,1,2],
        (x + i) > 0 && (y + j) > 0 && (x + i) <= size && (y + j) <= size
        && (abs i) /= (abs j)]

path :: Int -> ROBDDState (Int,Int) -> [(Int,Int)] -> ROBDDState (Int,Int)
path size ref nodes =
  do
  ref' <- ref
  let succLst = concat $ map (getSucc size) nodes
      result  = ref .&. (foldl1 (.&.) $ map singletonC succLst)
  result' <- result
  if result' == ref' then
    result
  else
    path size result succLst


doit :: Int -> ROBDDState (Int,Int)
doit n =
  do
  let start = singletonC (1,1)
  path n start [(1,1)]
