module Data.HBDD.ROBDDState
(
ROBDDState
, singletonS
, notS
, (.|.)
, (..|..)
, orS
, (.&.)
, (..&..)
, andS
, (.^.)
, xorS
, (.=>.)
, impS
, (.<=>.)
, equivS
)
where

import Control.Monad.Trans.State.Strict

import Prelude hiding(and, or, not)
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDFactory
import Data.HBDD.Operations

type ROBDDState v = State (ROBDDContext v) (ROBDD v)

singletonS :: Ord v => v -> ROBDDState v
singletonS var = do
                 ctx <- get
                 let (ctx', val) = singleton ctx var
                 put ctx'
                 return val

-- stateful computations
notS :: Ord v => ROBDD v -> ROBDDState v
notS = wrapUnary not

(.|.) :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
(.|.) = orS

(..|..) :: Ord v => ROBDDState v -> ROBDDState v -> ROBDDState v
a ..|.. b = do
            a' <- a
            b' <- b
            a' .|. b'

orS :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
orS = wrapBinary or

(.&.) :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
(.&.) = andS

(..&..) :: Ord v => ROBDDState v -> ROBDDState v -> ROBDDState v
a ..&.. b = do
            a' <- a
            b' <- b
            a' .&. b'
          

andS :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
andS = wrapBinary and

(.^.) :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
(.^.) = xorS

xorS :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
xorS = wrapBinary xor

(.=>.) :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
(.=>.) = impS

impS :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
impS = wrapBinary implies

(.<=>.) :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
(.<=>.) = equivS

equivS :: Ord v => ROBDD v -> ROBDD v -> ROBDDState v
equivS = wrapBinary equiv

-- FIXME: this pattern seems so common than there must be some functions doing that already
wrapBinary :: Ord v => (ROBDDContext v -> ROBDD v -> ROBDD v -> (ROBDDContext v, ROBDD v)) ->
                       ROBDD v -> ROBDD v -> ROBDDState v
wrapBinary f arg1 arg2 = do
                         ctx <- get
                         let (ctx', res) = f ctx arg1 arg2
                         put ctx'
                         return res

wrapUnary :: Ord v => (ROBDDContext v -> ROBDD v -> (ROBDDContext v, ROBDD v)) ->
                      ROBDD v -> ROBDDState v
wrapUnary f arg1= do
                  ctx <- get
                  let (ctx', res) = f ctx arg1
                  put ctx'
                  return res
