-- import Data.HBDD.ROBDD()
-- import Data.HBDD.ROBDDContext
-- import Data.HBDD.ROBDDFactory
-- import Data.HBDD.Operations
import Control.Monad.Trans.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Data.HBDD.ROBDDDot
import Prelude hiding(and,or)

main :: IO ()
main = putStrLn $ showDot context bdd
       where
       (bdd, context) = runState doit mkContext

doit :: ROBDDState Int
doit = do
       a <- singletonS 1
       b <- singletonS 2
       c <- singletonS 3
       a_and_b <- a .&. b
       a_and_c <- a .&. c
       a_and_b .|. a_and_c
