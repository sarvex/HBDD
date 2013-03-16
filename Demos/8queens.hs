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
       --putStrLn $ showDot context bdd
       putStrLn $ show $ getSat context bdd
       where
       (bdd, context) = runState (doit 4) mkContext -- FIXME: 1 reine

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
inSight :: (Int,Int) -> (Int,Int) -> Bool
inSight (x,y) (i,j) =
  ((x,y) /= (i,j)) && (x==i || y==j || (x+y) == (i+j) || (x-y) == (i-j))

doit :: Int -> ROBDDState (Int,Int)
doit n =
      do
       var_lst <- mapM singletonC [(i,j) | i <- [1..n], j <- [1..n]]
       rule_lst <-
             mapM (\v -> do
                   let matchLst = filter (\elt -> inSight (var v) (var elt)) var_lst
                   negExp <- foldl (\acc elt -> acc .|. elt) (return Zero) matchLst
                   v .&. notC(negExp))
                  var_lst
       -- not general
       foldl1 (\acc elt -> acc .|. elt) [(a.&. b .&. c .&. d) | a <- rule_lst, b <- rule_lst, c <- rule_lst, d <- rule_lst
                                                                , a /= b && a /= c && a /= d
                                                                  && b /= c && b /= d
                                                                  && c /= d]
