{-# LANGUAGE BangPatterns             #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.HBDD.ROBDD
(
ROBDD(..)
, identifier
, size
, leafToBool
, boolToLeaf
)
where

import Data.HBDD.UIDGenerator

-- | The type of ROBDDs. It can be either:
--     * 'ROBDD' a non-leaf node.
--     * 'ROBDDRef' a reference to an 'ROBDD'. Since we dont have any (pure) pointers, we have to
--     simulate them using this constructor. An 'ROBDDRef' as meaningless without the
--     'ROBDDContext' it has been created on. Thus, never use the same 'ROBDDRef' with two
--     differents contexts. This might lead to an assertion failure, or any other weird behaviour.
--     * 'Zero' the leaf equal to False.
--     * 'One' the leaf equal to True.
data ROBDD v = ROBDD (ROBDD v) v (ROBDD v) !UID Int
               | ROBDDRef !UID v !UID !UID Int
               | Zero
               | One
               deriving Show

-- | The identifier of a ROBDD. Two ROBDDs are equal iff they have the same identifier and were
-- created with the same 'ROBDDContext'.
identifier :: ROBDD v -> UID
identifier (ROBDD    _ _ _ i _) = i
identifier (ROBDDRef _ _ _ i _) = i
identifier Zero                 = 0
identifier One                  = 1

size :: ROBDD v -> Int
size (ROBDD    _ _ _ _ s) = s
size (ROBDDRef _ _ _ _ s) = s
size Zero                 = 1
size One                  = 1

instance Eq (ROBDD v) where
  a == b = identifier a == identifier b

-- | Converts a ROBDD Leaf ('Zero' or 'One') to a 'Boolean'. Returns 'undefined' if the given robdd
-- is not a leaf.
leafToBool :: Ord v => ROBDD v -> Bool
leafToBool One  = True
leafToBool Zero = False
leafToBool _    = undefined

-- | Converts a 'Boolean' to a ROBDD Leaf ('Zero' or 'One').
boolToLeaf :: Bool -> ROBDD v
boolToLeaf True  = One
boolToLeaf False = Zero

