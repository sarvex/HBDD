{-# LANGUAGE BangPatterns             #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
import qualified Data.IntMap as IM
import Data.HBDD.ComparableOp(ComparableOp)

import Debug.Trace

type IntAMap    k v = IM.IntMap (IM.IntMap (M.Map k (ROBDD v)))
type ROBDDId      v = (UID, v, UID)
type ROBDDOpId    v = (UID, ComparableOp, UID)
data ROBDDContext v = ROBDDContext !UIDGenerator !(IntAMap v v) !(IntAMap ComparableOp v)
                      deriving Show

mkContext :: ROBDDContext v
mkContext = ROBDDContext mkGenerator IM.empty IM.empty

allocId :: ROBDDContext v -> (UID, ROBDDContext v)
allocId (ROBDDContext generator c o) = let (res, generator') = UIDG.allocId generator in
                                     (res, ROBDDContext generator' c o)

lookup :: Ord v => ROBDDId v -> ROBDDContext v -> Maybe (ROBDD v)
lookup uid (ROBDDContext _ context _) = iamlookup uid context

lookupUnsafe :: Ord v => ROBDDId v -> ROBDDContext v -> ROBDD v
-- lookupUnsafe uid ctx = let Just res = lookup uid ctx in res
lookupUnsafe uid (ROBDDContext _ context _) = let Just res = iamlookup uid context in res

insert :: Ord v => ROBDDId v -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insert uid t (ROBDDContext i context o) = ROBDDContext i (iaminsert uid t context) o

lookupOp :: Ord v => ROBDDOpId v -> ROBDDContext v -> Maybe (ROBDD v)
lookupOp uid (ROBDDContext _ _ o) = iamlookup uid o

insertOp :: Ord v => ROBDDOpId v -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insertOp uid t (ROBDDContext i c o) = ROBDDContext i c (iaminsert uid t o)

-- Checks if a ROBDD is a singleton
isSingleton :: Ord v => ROBDDContext v -> ROBDD v -> Bool

isSingleton _ (ROBDD left _ right _)
  | (left == Zero || left == One) && (right == Zero || right == One) = True

isSingleton context (ROBDDRef left v right _) =
  isSingleton context $ lookupUnsafe (left,v,right) context

isSingleton _ _ = False

iamlookup  :: (Ord a, Ord v) => (Int, a, Int) -> IntAMap a v -> Maybe (ROBDD v)
iamlookup (i1, k, i2) m = IM.lookup i1 m >>= IM.lookup i2 >>= M.lookup k

iaminsert  :: (Ord a, Ord v) => (Int, a, Int) -> ROBDD v -> IntAMap a v -> IntAMap a v
iaminsert (i1, k, i2) val m = IM.alter (\mp -> Just $                                -- FIXME: ugly
                                        IM.alter (\mp' -> Just $
                                                  M.insert k val $
                                                  case mp' of
                                                  Just m'' -> m''
                                                  Nothing  -> M.empty) i2 $
                                        case mp of
                                        Just m' -> m'
                                        Nothing -> IM.empty)
                                       i1
                                       m
