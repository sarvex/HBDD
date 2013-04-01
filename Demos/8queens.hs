{-# LANGUAGE DoAndIfThenElse #-}

import System.Environment
import Data.HBDD.ROBDD
import Data.HBDD.Operations
import Control.Monad.Trans.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Prelude hiding(and,or,not)

usage :: String
usage = "8queens Demo : hbdd-8queens {-sat|-count|-satlist} n\n\tWhere n is the size of the board"

main :: IO ()
main = do
       args <- getArgs
       if length args /= 2 then
         putStrLn usage
       else
         let n              = read $ head $ tail $ args in
         let (bdd, context) = runState (doit n) mkContext in
         case head args of
           "-sat"     -> putStrLn $ show $ getSat     context bdd
           "-count"   -> putStrLn $ show $ satCount   context bdd
           "-satlist" -> putStrLn $ show $ getSatList context bdd
           _          -> putStrLn usage

-- Checks if the cell (i,j) is forbidden if there is a queen in cell (x,y),
-- returns True is so.
inSight :: Int -> (Int,Int) -> ROBDDState (Int,Int)
inSight n (x,y) =
  do
  let invalid = (\(i,j) -> ((x,y) /= (i,j))
                           && ((x ==i) || (y==j) || (x+y)==(i+j) || (x-y) == (i-j)))
      cannot_exist = (foldr1 (.&.)
        (map (singletonNotC) (filter invalid [(i,j) | i <- [1..n], j <- [1..n]])))
  (singletonC (x,y)) .=>. cannot_exist

doit :: Int -> ROBDDState (Int,Int)
doit n =
      do
       let queen_lst = foldr (\i acc ->
                 acc .&.
                 (foldr (\j acc1 -> (singletonC (i,j)) .|. acc1) (return Zero) [1..n]))
               (return One) [1..n]
       foldr (.&.) queen_lst [inSight n (i,j) | i <- [1..n], j <- [1..n]]
