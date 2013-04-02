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
, satCount
, getSatList
, restrict
, exists
, replace
)
where
{- operations.hs
 - This file contains the set of operations appliable on ROBDDs
 -}

import Prelude hiding (and, or, not)
import qualified Prelude as P
import Control.Monad
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory

type UnaryOp = Bool -> Bool
type BinOp   = Bool -> Bool -> Bool

-- | Generic function to apply unary logical operations on ROBDD. Prelude’s boolean operations can
-- be directly used as the first argument. Needs an explicit 'ROBDDContext' context passing.
unaryApply :: Ord v => UnaryOp -> ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)
unaryApply fn context (ROBDD left var right _ _) =
  let (leftContext, resLeft)   = unaryApply fn context left
      (rightContext, resRight) = unaryApply fn leftContext right
      in mkNode rightContext resLeft var resRight

unaryApply fn context (ROBDDRef left var right _ _) =
  unaryApply fn context (lookupUnsafe (ROBDDId left var right) context)

unaryApply fn context a =
  (context, boolToLeaf $ fn $ leafToBool a)

applyRec :: Ord v => BinOp -> ROBDDContext v -> v -> ROBDD v -> ROBDD v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
applyRec fn context var left right left' right'=
    let (leftContext, resLeft)   = apply fn context left left'
        (rightContext, resRight) = apply fn leftContext right right'
    in mkNode rightContext resLeft var resRight

-- | Generic function to apply binary logical operations on ROBDD. Prelude’s boolean operations can
-- be directly used as the first argument. Needs an explicit 'ROBDDContext' context passing.
apply :: Ord v => BinOp -> ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)

apply fn context leftTree@(ROBDD left var right _ _) rightTree@(ROBDD left' var' right' _ _) =
  let opId = ROBDDOpId (identifier leftTree) (identifier rightTree) in
  case lookupOp opId context of
  Just o  -> (context, o)
  Nothing -> let (ctx, res) = case compare var var' of
                              EQ -> applyRec fn context var left right left' right'
                              LT -> applyRec fn context var left right rightTree rightTree
                              GT -> applyRec fn context var' leftTree leftTree left' right'
             in
             (insertOp opId res ctx, res)

apply fn context (ROBDDRef left var right _ _) rightTree =
  apply fn context (lookupUnsafe (ROBDDId left var right) context) rightTree

apply fn context leftTree (ROBDDRef left var right _ _) =
  apply fn context leftTree (lookupUnsafe (ROBDDId left var right) context)

apply fn context Zero (ROBDD left var right _ _) =
  applyRec fn context var Zero Zero left right
apply fn context One (ROBDD left var right _ _) =
  applyRec fn context var One One left right

apply fn context (ROBDD left var right _ _) Zero =
  applyRec fn context var left right Zero Zero
apply fn context (ROBDD left var right _ _) One =
  applyRec fn context var left right One One

apply fn context a b =
  (context, boolToLeaf $ leafToBool a `fn` leafToBool b)

-- | NOT operator on ROBDD. Needs an explicit 'ROBDDContext' context passing. Use 'notC' instead
-- to get an implicit context passing.
not :: Ord v => ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)
not = unaryApply (P.not)

-- | AND operator on ROBDD. Needs an explicit 'ROBDDContext' context passing. Use 'andC' or
-- '(.&&.)' instead to get an implicit context passing.
and :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
and = apply (&&)

-- | OR operator on ROBDD. Needs an explicit 'ROBDDContext' context passing. Use 'orC' or
-- '(.||.)' instead to get an implicit context passing.
or :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
or = apply (||)

-- | XOR operator on ROBDD. Needs an explicit 'ROBDDContext' context passing. Use 'xorC' or
-- instead to get an implicit context passing.
xor :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
xor = apply (/=)

-- | Implication operator on ROBDD. Needs an explicit 'ROBDDContext' context passing. Use
-- 'impliesC' or '(.=>.)' instead to get an implicit context passing.
implies :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
implies = apply $ (||) . P.not

-- | Equivalence operator on ROBDD. Needs an explicit 'ROBDDContext' context passing. Use
-- 'equivC' or '(.<=>.)' instead to get an implicit context passing.
equiv :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
equiv = apply (==)

-- Interactions with ROBDD

-- | Compute a satisfying formula on the ROBDD. Returns 'Nothing' if no solutions ar found and Just
-- if there is at leas one solution. The solution is given as a list of 'Right' v for 'True'
-- variables, and 'Left' v for 'False' variables.
getSat :: Ord v => ROBDDContext v -> ROBDD v -> Maybe [Either v v]
getSat _ Zero = Nothing
getSat _ One = Just []

getSat context (ROBDD left v right _ _) =
  (getSat context left >>= return.(Left v:)) `mplus` (getSat context right >>= return.(Right v:))

getSat context (ROBDDRef left v right _ _) =
  getSat context $ lookupUnsafe (ROBDDId left v right) context

-- | Computes all satisfying formulas on the ROBDD. Returns an empty list if no solutions are
-- found, and a list of solution otherwise. The solutions have the same format as for the 'getSat'
-- operator.
getSatList :: Ord v => ROBDDContext v -> ROBDD v -> [[Either v v]]
getSatList  _ Zero = []
getSatList _ One = [[]]

getSatList context (ROBDD left var right _ _) =
  (map (Left  var:) $ getSatList context left)
  ++ (map (Right var:) $ getSatList context right)

getSatList context (ROBDDRef left v right _ _) =
  getSatList context $ lookupUnsafe (ROBDDId left v right) context

-- | Counts the number of satifying formulations on the ROBDD.
satCount :: Ord v => ROBDDContext v -> ROBDD v -> Int
satCount _ One = 1
satCount _ Zero = 0
satCount ctx (ROBDD left _ right _ _) = satCount ctx left + satCount ctx right
satCount ctx (ROBDDRef left v right _ _) = satCount ctx $ lookupUnsafe (ROBDDId left v right) ctx

-- Restrict function
restrict :: Ord v => ROBDDContext v -> v -> Bool -> ROBDD v -> (ROBDDContext v, ROBDD v)
restrict context var value (ROBDD left v right _ _) =
  if var == v then
    let direction = if value then left else right
    in restrict context var value direction
  else
    let (leftContext,leftRes) = restrict context var value left
        (rightContext, rightRes) = restrict leftContext var value right in
    mkNode rightContext leftRes v rightRes

restrict context var value (ROBDDRef left v right _ _) =
  restrict context var value $ lookupUnsafe (ROBDDId left v right) context

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
exists context var@(ROBDD _ v _ _ _) node
  | isSingleton context var =
    exists' context v node

exists context (ROBDDRef left v right _ _) node =
  exists context (lookupUnsafe (ROBDDId left v right) context) node

exists _ _ _ = undefined

-- | Replaces a variable with another in a BDD. Needs explicit context passing. Use 'replaceC'
-- instead to get implicit context passing.
replace :: Ord v => ROBDDContext v -> ROBDD v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)
replace context rep@(ROBDD _ v _ _ _) with bdd
  | (isSingleton context rep) && (isSingleton context with) =
    let (ctx,res0)      = restrict context v False bdd
        (ctx',res1)     = restrict ctx v True bdd
        (ctx'',notwith) = not ctx' rep
        (ctx1,ret1)     = and ctx'' with res1
        (ctx2,ret2)     = and ctx1 notwith res0 in
        or ctx2 ret1 ret2

replace context (ROBDDRef left v right _ _) with bdd =
  replace context (lookupUnsafe (ROBDDId left v right) context) with bdd

replace context rep (ROBDDRef left v right _ _) bdd =
  replace context rep (lookupUnsafe (ROBDDId left v right) context) bdd

replace _ _ _ _ = undefined
