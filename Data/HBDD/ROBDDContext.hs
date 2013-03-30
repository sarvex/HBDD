{-# LANGUAGE BangPatterns             #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.HBDD.ROBDDContext
(
ROBDDId(..)
, ROBDDOpId(..)
, ROBDDContext
, mkContext
, clearOpContext
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
import qualified Data.IntMap.Strict as IM
import Data.HBDD.ComparableOp(ComparableOp)

import Debug.Trace

type IntAMap    k v = IM.IntMap (IM.IntMap (M.Map k (ROBDD v)))
data ROBDDId      v = ROBDDId !UID v !UID
data ROBDDOpId      = ROBDDOpId !UID ComparableOp !UID
data ROBDDContext v = ROBDDContext !UIDGenerator !(IntAMap v v) !(IntAMap ComparableOp v)
                      deriving Show

mkContext :: ROBDDContext v
mkContext = ROBDDContext mkGenerator IM.empty IM.empty

clearOpContext :: ROBDDContext v -> ROBDDContext v
clearOpContext (ROBDDContext gen nc o) = ROBDDContext gen nc IM.empty

allocId :: ROBDDContext v -> (UID, ROBDDContext v)
allocId (ROBDDContext generator c o) = let (res, generator') = UIDG.allocId generator in
                                     (res, ROBDDContext generator' c o)

lookup :: Ord v => ROBDDId v -> ROBDDContext v -> Maybe (ROBDD v)
lookup (ROBDDId ia k ib) (ROBDDContext _ context _) = iamlookup ia k ib context

lookupUnsafe :: Ord v => ROBDDId v -> ROBDDContext v -> ROBDD v
-- lookupUnsafe uid ctx = let Just res = lookup uid ctx in res
lookupUnsafe (ROBDDId ia k ib) (ROBDDContext _ context _) = let Just res = iamlookup ia k ib context in res

insert :: Ord v => ROBDDId v -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insert (ROBDDId ia k ib) t (ROBDDContext i context o) = ROBDDContext i (iaminsert ia k ib t context) o

lookupOp :: Ord v => ROBDDOpId -> ROBDDContext v -> Maybe (ROBDD v)
lookupOp (ROBDDOpId ia k ib) (ROBDDContext _ _ o) = iamlookup ia k ib o

insertOp :: Ord v => ROBDDOpId -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insertOp (ROBDDOpId ia k ib) t (ROBDDContext i c o) = ROBDDContext i c (iaminsert ia k ib t o)

-- Checks if a ROBDD is a singleton
isSingleton :: Ord v => ROBDDContext v -> ROBDD v -> Bool

isSingleton _ (ROBDD left _ right _)
  | (left == Zero || left == One) && (right == Zero || right == One) = True

isSingleton context (ROBDDRef left v right _) =
  isSingleton context $ lookupUnsafe (ROBDDId left v right) context

isSingleton _ _ = False

iamlookup  :: (Ord a, Ord v) => Int -> a -> Int -> IntAMap a v -> Maybe (ROBDD v)
iamlookup i1 k i2 m = IM.lookup i1 m >>= IM.lookup i2 >>= M.lookup k

iaminsert  :: (Ord a, Ord v) => Int -> a -> Int -> ROBDD v -> IntAMap a v -> IntAMap a v
iaminsert i1 k i2 val m = IM.alter (\mp -> Just $                                -- FIXME: ugly
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
