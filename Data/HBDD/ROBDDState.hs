{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Data.HBDD.ROBDDState
(
ROBDDState
, singletonC
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
)
where

import Control.Monad.Trans.State.Strict

import Prelude hiding(and, or, not)
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations

type ROBDDState v = State (ROBDDContext v) (ROBDD v)

class Ord v => ROBDDBinOp a b v | a b -> v where
  rewrite ::(ROBDD v -> ROBDD v -> ROBDDState v) -> a -> b -> ROBDDState v

  andC :: a -> b -> ROBDDState v
  andC = rewrite andC
  (.&.) :: a -> b -> ROBDDState v
  (.&.) = andC

  orC :: a -> b -> ROBDDState v
  orC = rewrite orC
  (.|.) :: a -> b -> ROBDDState v
  (.|.) = orC

  xorC :: a -> b -> ROBDDState v
  xorC = rewrite xorC
  (.^.) :: a -> b -> ROBDDState v
  (.^.) = xorC

  impC :: a -> b -> ROBDDState v
  impC = rewrite impC
  (.=>.) :: a -> b -> ROBDDState v
  (.=>.) = impC

  equivC :: a -> b -> ROBDDState v
  equivC = rewrite equivC
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

class Ord v => ROBDDUnOp a v | a -> v where
  notC :: a -> ROBDDState v

instance Ord v => ROBDDUnOp (ROBDD v) v where
  notC = wrapUnary not

instance Ord v => ROBDDUnOp (ROBDDState v) v where
  notC a = do
    a' <- a
    notC a'

singletonC :: Ord v => v -> ROBDDState v
singletonC var = do
                 ctx <- get
                 let (ctx', val) = singleton ctx var
                 put ctx'
                 return $! val

-- FIXME: this pattern seems so common than there must be some functions doing that already
wrapBinary :: Ord v => (ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)) ->
                       ROBDD v -> ROBDD v -> ROBDDState v
wrapBinary f arg1 arg2 = do
                         ctx <- get
                         let (ctx', res) = f ctx arg1 arg2
                         put $! clearOpContext ctx'
                         return $! res

wrapUnary :: Ord v => (ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)) ->
                      ROBDD v -> ROBDDState v
wrapUnary f arg1= do
                  ctx <- get
                  let (ctx', res) = f ctx arg1
                  put $! clearOpContext ctx'
                  return $! res
