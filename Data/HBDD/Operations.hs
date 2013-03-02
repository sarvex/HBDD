module Data.HBDD.Operations
(
UnaryOp
, BinOp
, unaryApply
, apply
, and
, or
, xor
, equiv
)
where
{- operations.hs
 - This file contains the set of operations appliable on ROBDDS
 -}

import Prelude hiding (and, or)
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory

type UnaryOp = Bool -> Bool
type BinOp = Bool -> Bool -> Bool

unaryApply :: Ord v => UnaryOp -> ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)
unaryApply fn context (ROBDD left var right _) =
  let (leftContext, resLeft)   = unaryApply fn context left
      (rightContext, resRight) = unaryApply fn leftContext right
      in mkNode rightContext resLeft var resRight

unaryApply fn context (ROBDDRef left var right _) =
  unaryApply fn context (lookupUnsafe (left,var,right) context)

unaryApply fn context a =
  (context, boolToLeaf $ fn $ leafToBool a)

applyRec :: Ord v => BinOp -> ROBDDContext v -> v -> ROBDD v -> ROBDD v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
applyRec fn context var r1 r2 r3 r4 =
    let (leftContext, resLeft)   = apply fn context r1 r3
        (rightContext, resRight) = apply fn leftContext r2 r4
    in mkNode rightContext resLeft var resRight

apply :: Ord v => BinOp -> ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)

apply fn context Zero (ROBDD left var right _) =
  applyRec fn context var Zero Zero left right
apply fn context One (ROBDD left var right _) =
  applyRec fn context var One One left right

apply fn context (ROBDD left var right _) Zero =
  applyRec fn context var left right Zero Zero
apply fn context (ROBDD left var right _) One =
  applyRec fn context var left right One One

apply fn context (ROBDDRef left var right _) rightTree =
  apply fn context (lookupUnsafe (left,var,right) context) rightTree

apply fn context leftTree (ROBDDRef left var right _) =
  apply fn context leftTree (lookupUnsafe (left,var,right) context)


apply fn context leftTree@(ROBDD left var right _) rightTree@(ROBDD left' var' right' _) =
  case compare var var' of
    EQ -> applyRec fn context var left right left' right'
    LT -> applyRec fn context var left right rightTree rightTree
    GT -> applyRec fn context var' leftTree leftTree left' right'


apply fn context a b =
  (context, boolToLeaf $ leafToBool a `fn` leafToBool b)

leafToBool :: Ord v => ROBDD v -> Bool
leafToBool One = True
leafToBool Zero = False
leafToBool _ = undefined

boolToLeaf :: Bool -> ROBDD v
boolToLeaf True = One
boolToLeaf False = Zero

-- FIXME: wait for unary apply prototype
-- not :: ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)
-- not = apply_one (not)

and :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
and = apply (&&)

or :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
or = apply (||)

xor :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
xor = apply (\ left right -> (left || right) && (not $ left && right))

equiv :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
equiv = apply (\ left right -> (left && right) || (not $ left && right))

-- exists :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
-- exists = apply (&&)
--
-- forall :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
-- forall = apply (&&)


{- operations.hs ends here -}
