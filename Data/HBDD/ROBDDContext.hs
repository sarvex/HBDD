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

-- | A (Int, Int, a) map. Chaining three maps seems to be a lot faster than using a single
-- triplet-indexed map.
type IntAMap      k v = IM.IntMap (IM.IntMap (M.Map k (ROBDD v)))
-- | A (Int, Int) map. Chaining two maps seems to be a lot faster than using a single
-- pair-indexed map.
type IntIntMap    v = IM.IntMap (IM.IntMap (ROBDD v))

-- | The way ROBDDs are identified inside of the ROBDD cache.
data ROBDDId      v = ROBDDId   !UID v !UID

-- | The way operations are identified inside of the operations cach.
data ROBDDOpId      = ROBDDOpId !UID !UID

-- | The robdd context. Since this is a pure library, there is no global context. Instead, one must
-- create a new context using 'mkContext', and pass it to the operators.
data ROBDDContext v = ROBDDContext !UIDGenerator !(IntAMap v v) !(IntIntMap v)
                      deriving Show

-- | Creates a new, empty context. The same context must be used for all operations as long as
-- they need reduceness of the BDD. A context is also the only way to get the real 'ROBDD' from a
-- 'ROBDDRef'. The context contains two caches: an ROBDD cache, ensuring the node-sharing of the
-- formulaes’s BDD, and an operation cache improving binary operation computation times. Since the
-- operations cache does not keep track of which operations is applies, it must _always_ be cleared
-- with the 'clearOpContext' between each operations. Failing to clear the operation cache may lead
-- to unpredictable behaviour.
--
-- Having to pass the ROBDDContext around, and to clean the operations cache every time is a bit
-- laborious. To avoid explicit passing and cleanning, use the State interface to manipulate
-- ROBDDs safely.
mkContext :: ROBDDContext v
mkContext = ROBDDContext mkGenerator IM.empty IM.empty

-- | Clears the operations cache. The cache does not have any limit, but is intented to be used for
-- one operation only. Thus, it must always be called in-between two operations.
clearOpContext :: ROBDDContext v -> ROBDDContext v
clearOpContext (ROBDDContext gen nc _) = ROBDDContext gen nc IM.empty

-- | Allocate a new identifier for a newly cleated ROBDD.
allocId :: ROBDDContext v -> (UID, ROBDDContext v)
allocId (ROBDDContext generator c o) = let (res, generator') = UIDG.allocId generator in
                                     (res, ROBDDContext generator' c o)

-- | Tries to find an robdd . Use this to get an ROBDD from an ROBDDRef.
lookup :: Ord v => ROBDDId v -> ROBDDContext v -> Maybe (ROBDD v)
lookup (ROBDDId ia k ib) (ROBDDContext _ context _) = iamlookup ia k ib context

-- | Same as 'lookup' but fails in case of mismatch.
lookupUnsafe :: Ord v => ROBDDId v -> ROBDDContext v -> ROBDD v
lookupUnsafe (ROBDDId ia k ib) (ROBDDContext _ context _) = let Just res = iamlookup ia k ib context in res

-- | Add a ROBDD to the ROBDD cache.
insert :: Ord v => ROBDDId v -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insert (ROBDDId ia k ib) t (ROBDDContext i context o) = ROBDDContext i (iaminsert ia k ib t context) o

-- | Tries to retrieve a previously cached result of a binary operation.
lookupOp :: Ord v => ROBDDOpId -> ROBDDContext v -> Maybe (ROBDD v)
lookupOp (ROBDDOpId ia ib) (ROBDDContext _ _ o) = iimlookup ia ib o

-- | Saves the result of a binary operation of 
insertOp :: Ord v => ROBDDOpId -> ROBDD v -> ROBDDContext v -> ROBDDContext v
insertOp (ROBDDOpId ia ib) t (ROBDDContext i c o) = ROBDDContext i c (iiminsert ia ib t o)

-- | Checks if a ROBDD is a singleton.
isSingleton :: Ord v => ROBDDContext v -> ROBDD v -> Bool

isSingleton _ (ROBDD left _ right _ _)
  | (left == Zero || left == One) && (right == Zero || right == One) = True

isSingleton context (ROBDDRef left v right _ _) =
  isSingleton context $ lookupUnsafe (ROBDDId left v right) context

isSingleton _ _ = False

-- | Lookup on an IntIntMap.
iimlookup  :: (Ord v) => Int -> Int -> IntIntMap v -> Maybe (ROBDD v)
iimlookup i1 i2 m = IM.lookup i1 m >>= IM.lookup i2

-- | Lookup on an IntAMap.
iamlookup  :: (Ord a, Ord v) => Int -> a -> Int -> IntAMap a v -> Maybe (ROBDD v)
iamlookup i1 k i2 m = IM.lookup i1 m >>= IM.lookup i2 >>= M.lookup k

-- | Insersion on an IntAMap.
iaminsert  :: (Ord a, Ord v) => Int -> a -> Int -> ROBDD v -> IntAMap a v -> IntAMap a v
iaminsert i1 k i2 val m = IM.alter (\mp -> Just $
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

-- | Insersion on an IntIntMap.
iiminsert  :: (Ord v) => Int -> Int -> ROBDD v -> IntIntMap v -> IntIntMap v
iiminsert i1 i2 val m = IM.alter (\mp -> Just $
                                   IM.insert i2 val $
                                   case mp of
                                     Just m' -> m'
                                     Nothing -> IM.empty)
                                  i1
                                  m
