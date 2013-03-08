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
, exists
, replace
)
where
{- operations.hs
 - This file contains the set of operations appliable on ROBDDS
 -}

import Prelude hiding (and, or, not)
import qualified Prelude as P
import Control.Monad
import Data.Maybe
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
implies = apply $ (||) . P.not

equiv :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
equiv = apply (==)

-- Interactions with ROBDD

-- Returns a satisfying formula on the ROBDD
getSat :: Ord v => ROBDDContext v -> ROBDD v -> Maybe [v]
getSat _ Zero = Nothing
getSat _ One = Just []

getSat context (ROBDD left v right _) =
  (getSat context left `mplus` getSat context right) >>= return.(v:)

getSat context (ROBDDRef left v right _) =
  getSat context $ lookupUnsafe (left,v,right) context

-- Returns the list of satisfied formulas
getSatList :: Ord v => ROBDDContext v -> ROBDD v -> Maybe [[v]]
getSatList  _ Zero = Nothing
getSatList _ One = Just [[]]

-- FIXME: indent, remove { and }
getSatList context (ROBDD left var right _) =
  let resList = concat $ catMaybes [getSatList context left, getSatList context right]
  in
  if null resList then Nothing
  else Just $ map (var:) resList

getSatList context (ROBDDRef left v right _) =
  getSatList context $ lookupUnsafe (left,v,right) context


-- Restrict function
restrict :: Ord v => ROBDDContext v -> v -> Bool -> ROBDD v -> (ROBDDContext v, ROBDD v)

restrict context var value (ROBDD left v right _) =
  if var == v then
    if value then (context, left) else (context,right)
  else
    let (leftContext,leftRes) = restrict context var value left
        (rightContext, rightRes) = restrict leftContext var value right in
    mkNode rightContext leftRes v rightRes

restrict context var value (ROBDDRef left v right _) =
  restrict context var value $ lookupUnsafe (left,v,right) context

---- The remaining cases are only the leaves (One/Zero)
restrict context _ _ leaf = (context,leaf)

-- Exists : for a variable, one of the values of the variables yields a
-- satisfiable ROBDD

exists' :: Ord v => ROBDDContext v -> v -> ROBDD v -> (ROBDDContext v, ROBDD v)
exists' context var robdd =
  let (leftC,leftRes)   = restrict context var False robdd
      (rightC,rightRes) = restrict leftC var True robdd
  in
    or rightC leftRes rightRes

exists :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
exists context var@(ROBDD _ v _ _) node
  | isSingleton context var =
    exists' context v node

exists context (ROBDDRef left v right _) node =
  exists context (lookupUnsafe (left,v,right) context) node

exists _ _ _ = undefined
-- forall :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
-- forall = apply (&&)

-- Replace : replace a variable with another in a BDD

replace' :: Ord v => ROBDDContext v -> v -> v -> ROBDD v -> (ROBDDContext v, ROBDD v)
replace' context rep with (ROBDD left v right _) =
    let (leftC, leftRes)   = replace' context rep with left
        (rightC, rightRes) = replace' leftC rep with right
  in mkNode rightC leftRes rep_var rightRes
  where rep_var = if rep == v then with else v

replace' context rep with (ROBDDRef left v right _) =
  replace' context rep with $ lookupUnsafe (left,v,right) context

replace' context _ _ b = (context,b)

replace :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
replace context rep@(ROBDD _ v _ _) with@(ROBDD _ v' _ _) bdd
  | (isSingleton context rep) && (isSingleton context with) =
    replace' context v v' bdd

replace context (ROBDDRef left v right _) with bdd =
  replace context (lookupUnsafe (left,v,right) context) with bdd
replace context rep (ROBDDRef left v right _) bdd =
  replace context rep (lookupUnsafe (left,v,right) context) bdd

replace _ _ _ _ = undefined

{- operations.hs ends here -}
