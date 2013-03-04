module Data.HBDD.Operations
(
UnaryOp
, BinOp
, unaryApply
, apply
, not
, and
, or
, xor
, equiv
, implies
, getSat
, getSatList
)
where
{- operations.hs
 - This file contains the set of operations appliable on ROBDDS
 -}

import Prelude hiding (and, or, not)
import qualified Prelude as P
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory

type UnaryOp = Bool -> Bool
type BinOp = Bool -> Bool -> Bool

-- Generic function for unary logical operations on ROBDD
unaryApply :: Ord v => UnaryOp -> ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)
unaryApply fn context (ROBDD left var right _) =
  let (leftContext, resLeft)   = unaryApply fn context left
      (rightContext, resRight) = unaryApply fn leftContext right
      in mkNode rightContext resLeft var resRight

unaryApply fn context (ROBDDRef left var right _) =
  unaryApply fn context (lookupUnsafe (left,var,right) context)

unaryApply fn context a =
  (context, boolToLeaf $ fn $ leafToBool a)

-- Does the recursion when applying a binary operator
applyRec :: Ord v => BinOp -> ROBDDContext v -> v -> ROBDD v -> ROBDD v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
applyRec fn context var left right left' right'=
    let (leftContext, resLeft)   = apply fn context left left'
        (rightContext, resRight) = apply fn leftContext right right'
    in mkNode rightContext resLeft var resRight

-- Generic function for binary logical operations on ROBDD
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

-- Logical operations on ROBB
not :: Ord v => ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)
not = unaryApply (P.not)

and :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
and = apply (&&)

or :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
or = apply (||)

xor :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
xor = apply (/=)

implies :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
implies = apply (\ left right -> (P.not left) || right)

equiv :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
equiv = apply (==)

-- Interactions with ROBDD

-- Returns a satisfying formula on the ROBDD
getSat :: Ord v => ROBDDContext v -> ROBDD v -> Maybe [v]
getSat _ Zero = Nothing
getSat _ One = Just []

getSat context (ROBDD left v right _) =
  case (getSat context left, getSat context right) of
    (Just xs, _) -> Just $ v:xs
    (_, Just xs) -> Just $ v:xs
    (_,_)        -> Nothing

getSat context (ROBDDRef left v right _) =
  getSat context $ lookupUnsafe (left,v,right) context

-- Returns the list of satisfied formulas
getSatList :: Ord v => ROBDDContext v -> ROBDD v -> Maybe [[v]]
getSatList  _ Zero = Nothing
getSatList _ One = Just [[]]

-- FIXME: indent, remove { and }
getSatList context (ROBDD left var right _) =
  let {resList = case (getSatList context left, getSatList context right) of
    (Just ls, Just rs) -> concat [ls,rs]
    (Just ls, _)       -> ls
    (_, Just rs)       -> rs
    _                  -> []}
    in
      if null resList then Nothing
      else Just $ map (\ lst -> var:lst) resList

getSatList context (ROBDDRef left v right _) =
  getSatList context $ lookupUnsafe (left,v,right) context

-- exists :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
-- exists = apply (&&)
--
-- forall :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
-- forall = apply (&&)


{- operations.hs ends here -}
