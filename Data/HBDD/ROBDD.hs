{-# LANGUAGE BangPatterns             #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.HBDD.ROBDD
(
ROBDD(..)
, identifier
, leafToBool
, boolToLeaf
)
where

import Data.HBDD.UIDGenerator

data ROBDD v = ROBDD (ROBDD v) v (ROBDD v) !UID
               | ROBDDRef !UID v !UID !UID
               | Zero
               | One
               deriving Show

identifier :: ROBDD v -> UID
identifier (ROBDD    _ _ _ i) = i
identifier (ROBDDRef _ _ _ i) = i
identifier Zero               = 0
identifier One                = 1

instance Eq (ROBDD v) where
  a == b = identifier a == identifier b

-- Convert a ROBDD Leaf (Zero/One) to a Bool
leafToBool :: Ord v => ROBDD v -> Bool
leafToBool One = True
leafToBool Zero = False
leafToBool _ = undefined

-- Converts a Bool to a ROBDD
boolToLeaf :: Bool -> ROBDD v
boolToLeaf True = One
boolToLeaf False = Zero

