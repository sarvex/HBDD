module Data.HBDD.ComparableOp
(
ComparableOp(..)
, or
, and
, xor
, equiv
, implies
)
where

import Prelude hiding(or, and)
import qualified Prelude as P

data ComparableOp = ComparableOp (Bool -> Bool -> Bool) Int

instance Eq ComparableOp where
  (ComparableOp _ i) == (ComparableOp _ i') = i == i'

instance Ord ComparableOp where
  (ComparableOp _ i) >= (ComparableOp _ i') = i >= i'

instance Show ComparableOp where
  show (ComparableOp _ i) = show i

or :: ComparableOp
or = ComparableOp (||) 1

and :: ComparableOp
and = ComparableOp (&&) 2

xor :: ComparableOp
xor = ComparableOp (/=)  3

equiv :: ComparableOp
equiv = ComparableOp (==) 4

implies :: ComparableOp
implies = ComparableOp ((||) . P.not) 5
