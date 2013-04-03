{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Data.HBDD.ROBDDState
(
-- | This modure exports the State monad based interface for manipulating ROBDDs. It is a lot
-- easier to use than the explicit state passing api.
ROBDDState
, singletonC
, singletonNotC
, notC
, orC
, (.|.)
, andC
, (.&.)
, xorC
, (.^.)
, impC
, (.=>.)
, equivC
, (.<=>.)
, restrictC
, existsC
, replaceC
, foldBySizeC
)
where

import Control.Monad.Trans.State.Strict
import Control.Monad

import Prelude hiding(and, or, not)
import Data.List(insertBy, sortBy)
import Data.Function
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations

-- | Type of the ROBDD and its Context hidden inside of the State monad.
type ROBDDState v = State (ROBDDContext v) (ROBDD v)

-- | Class of binary operations acting on ROBDDs.
-- Minimim implementation requires the function 'rewrite' which intend to be an operator
-- used to rewrite binary operation to fit the new signature 'a -> b -> ROBDDState v'.
class Ord v => ROBDDBinOp a b v | a b -> v where
  rewrite ::(ROBDD v -> ROBDD v -> ROBDDState v) -> a -> b -> ROBDDState v

  -- | AND operator with implicit state passing.
  andC :: a -> b -> ROBDDState v
  andC = rewrite andC

  -- | AND operator with implicit state passing.
  (.&.) :: a -> b -> ROBDDState v
  (.&.) = andC

  -- | OR operator with implicit state passing.
  orC :: a -> b -> ROBDDState v
  orC = rewrite orC

  -- | OR operator with implicit state passing.
  (.|.) :: a -> b -> ROBDDState v
  (.|.) = orC

  -- | XOR operator with implicit state passing.
  xorC :: a -> b -> ROBDDState v
  xorC = rewrite xorC

  -- | XOR operator with implicit state passing.
  (.^.) :: a -> b -> ROBDDState v
  (.^.) = xorC

  -- | Implication operator with implicit state passing.
  impC :: a -> b -> ROBDDState v
  impC = rewrite impC

  -- | Implication operator with implicit state passing.
  (.=>.) :: a -> b -> ROBDDState v
  (.=>.) = impC

  -- | Equivalence operator with implicit state passing.
  equivC :: a -> b -> ROBDDState v
  equivC = rewrite equivC

  -- | Equivalence operator with implicit state passing.
  (.<=>.) :: a -> b -> ROBDDState v
  (.<=>.) = equivC


instance Ord v => ROBDDBinOp (ROBDD v) (ROBDD v) v where
  rewrite fn a b = fn a b
  andC   = rewrite $ wrapBinary and
  orC    = rewrite $ wrapBinary or
  xorC   = rewrite $ wrapBinary xor
  impC   = rewrite $ wrapBinary implies
  equivC = rewrite $ wrapBinary equiv

instance Ord v => ROBDDBinOp (ROBDDState v) (ROBDDState v) v where
  rewrite fn a b = do
    a' <- a
    b' <- b
    fn a' b'

instance Ord v => ROBDDBinOp (ROBDDState v) (ROBDD v) v where
  rewrite fn a  b = do
    a' <- a
    fn a' b

instance Ord v => ROBDDBinOp (ROBDD v) (ROBDDState v) v where
  rewrite fn a b = do
    b' <- b
    fn a b'

-- | Class of the negation operator working on obdds. It allowg the use of the same NotC operator
-- on lifted and unlifted ROBDDs.
class Ord v => ROBDDUnOp a v | a -> v where
  -- | NOT operator with implicit state passing.
  notC :: a -> ROBDDState v

instance Ord v => ROBDDUnOp (ROBDD v) v where
  notC = wrapUnary not

instance Ord v => ROBDDUnOp (ROBDDState v) v where
  notC a = do
    a' <- a
    notC a'

-- | Same as 'singleton' but inside of the State monad, avoiding explicit 'ROBDDContext'
-- passing.
singletonC :: Ord v => v -> ROBDDState v
singletonC var = do
                 ctx <- get
                 let (ctx', val) = singleton ctx var
                 put    $! clearOpContext ctx'
                 return $! val

-- | Same as 'singletonNot' but inside of the State monad, avoiding explicit 'ROBDDContext'
-- passing.
singletonNotC :: Ord v => v -> ROBDDState v
singletonNotC var = do
                    ctx <- get
                    let (ctx', val) = singletonNot ctx var
                    put    $! clearOpContext ctx'
                    return $! val

-- | Same as 'restrict' but inside of the State monad, avoiding explicit 'ROBDDContext' passing.
restrictC :: Ord v => v -> Bool -> ROBDDState v -> ROBDDState v
restrictC var value robdd =
  do
    ctx    <- get
    robdd' <- robdd
    let (ctx',val) = restrict ctx var value robdd'
    put    $! clearOpContext ctx'
    return $! val

-- | Same as 'exists' but inside of the State monad, avoiding explicit 'ROBDDContext' passing.
existsC :: Ord v => ROBDDState v -> ROBDDState v -> ROBDDState v
existsC var node =
  do
    ctx   <- get
    var'  <- var
    node' <- node
    let (ctx',val) = exists ctx var' node'
    put    $! clearOpContext ctx'
    return $! val

-- | Same as 'replace' but inside of the State monad, avoiding explicit 'ROBDDContext' passing.
replaceC :: Ord v => ROBDDState v -> ROBDDState v -> ROBDDState v -> ROBDDState v
replaceC rep with bdd =
    do
    ctx   <- get
    rep'  <- rep
    with' <- with
    bdd'  <- bdd
    let (ctx',val) = replace ctx rep' with' bdd'
    put    $! clearOpContext ctx'
    return $! val

-- | ROBDD binary operator folding on BDD with increasing sizes. This is a kind of
-- divid-and-conquer application of the binary operation on a list of ROBDD.
foldBySizeC :: Ord v => (ROBDDState v -> ROBDDState v -> ROBDDState v) -> [ ROBDDState v ] -> ROBDDState v
foldBySizeC f l = foldRec sorted
                  where
                  foldRec ls = ls >>= \ls' ->
                                      case ls' of
                                      []       -> error "List.empty"
                                      e:[]     -> return e
                                      e1:e2:es -> do
                                                  res <- f (return e1) (return e2)
                                                  foldRec $ return $ insertBy (compare `on` size) res es
                  sorted = (liftM2 sortBy) (return (compare `on` size)) (sequence l)

-- | Lifts a binary operator inside of the State monad.
wrapBinary :: Ord v => (ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)) ->
                       ROBDD v -> ROBDD v -> ROBDDState v
wrapBinary f arg1 arg2 = do
                         ctx <- get
                         let (ctx', res) = f ctx arg1 arg2
                         put    $! clearOpContext ctx'
                         return $! res

-- | Lifts an unary operator inside of the State monad.
wrapUnary :: Ord v => (ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)) ->
                      ROBDD v -> ROBDDState v
wrapUnary f arg1= do
                  ctx <- get
                  let (ctx', res) = f ctx arg1
                  put    $! clearOpContext ctx'
                  return $! res
