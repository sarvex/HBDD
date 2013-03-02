module Data.HBDD.ROBDD
(
ROBDD(..)
, identifier
)
where

import Data.HBDD.UIDGenerator

data ROBDD v = ROBDD (ROBDD v) v (ROBDD v) UID
               | ROBDDRef UID v UID UID
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

