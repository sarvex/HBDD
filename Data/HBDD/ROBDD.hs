module Data.HBDD.ROBDD
(
ROBDD(..)
)
where

import Data.HBDD.UIDGenerator

data ROBDD v = ROBDD (ROBDD v) v (ROBDD v) UID
               | ROBDDRef UID v UID
               | Zero
               | One
               deriving Show
