module Data.HBDD.ROBDDContext
(
ROBDDId
, ROBDDContext
, mkContext
, allocId
, lookup
, lookupUnsafe
, lookupOp
, insert
, insertOp
, isSingleton
)
where

import Prelude hiding(lookup)
import Data.HBDD.ROBDD
import Data.HBDD.UIDGenerator hiding(allocId)
import qualified Data.HBDD.UIDGenerator as UIDG
import qualified Data.Map.Strict as M
import Data.HBDD.ComparableOp(ComparableOp)

import Debug.Trace

type ROBDDId v = (UID, v, UID)
type ROBDDOpId v = (UID, ComparableOp, UID)
data ROBDDContext v = ROBDDContext UIDGenerator (M.Map (ROBDDId v) (ROBDD v)) (M.Map (ROBDDOpId v) (ROBDD v))
                      deriving Show

mkContext :: ROBDDContext v
mkContext = ROBDDContext mkGenerator M.empty M.empty

allocId :: ROBDDContext v -> (UID, ROBDDContext v)
allocId (ROBDDContext generator c o) = let (res, generator') = UIDG.allocId generator in
                                     (res, ROBDDContext generator' c o)

lookup :: Ord v => ROBDDId v -> ROBDDContext v -> Maybe (ROBDD v)
lookup uid (ROBDDContext _ context _) = M.lookup uid context

lookupUnsafe :: Ord v => ROBDDId v -> ROBDDContext v -> ROBDD v
-- lookupUnsafe uid ctx = let Just res = lookup uid ctx in res
lookupUnsafe uid (ROBDDContext _ context _) = let Just res = M.lookup uid context in res

insert :: Ord v => ROBDDId v -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insert uid t (ROBDDContext i context o) = ROBDDContext i (M.insert uid t context) o

lookupOp :: Ord v => ROBDDOpId v -> ROBDDContext v -> Maybe (ROBDD v)
lookupOp uid (ROBDDContext _ _ o) = M.lookup uid o

insertOp :: Ord v => ROBDDOpId v -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insertOp uid t (ROBDDContext i c o) = ROBDDContext i c (M.insert uid t o)

-- Checks if a ROBDD is a singleton
isSingleton :: Ord v => ROBDDContext v -> ROBDD v -> Bool

isSingleton _ (ROBDD left _ right _)
  | (left == Zero || left == One) && (right == Zero || right == One) = True

isSingleton context (ROBDDRef left v right _) =
  isSingleton context $ lookupUnsafe (left,v,right) context

isSingleton _ _ = False
