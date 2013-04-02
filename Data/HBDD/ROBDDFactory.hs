module Data.HBDD.ROBDDFactory
(
mkNode
, singleton
, singletonNot
)
where

import Prelude hiding(lookup)
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext

-- | Creates an new ROBDD from a variable and its two children. If the same ROBDD already exists
-- and the context, no node is created and an ROBDDRef is returned instead.
mkNode :: Ord v => ROBDDContext v -> ROBDD v -> v -> ROBDD v -> (ROBDDContext v, ROBDD v)
mkNode context l v r = case lookup (ROBDDId idl v idr) context of
                       Just c  -> (context, ROBDDRef idl v idr (identifier c) (size c))
                       Nothing ->
                         if idl == idr then
                          (context, l) -- If the left and right child are the same, no need to create a useless node
                         else
                           let (uid, ctx) = allocId context
                               res        = ROBDD l v r uid (size l + size r + 1)
                               ctx'       = insert (ROBDDId idl v idr) res ctx
                           in
                           (ctx', res)
                       where
                       idl = identifier l
                       idr = identifier r

-- | Creates a new singleton from a variable. If the same singleton already exists and the context,
-- no node is created and an ROBDDRef is returned instead.
-- Use 'singletonNotC' for an implicitly contextualized node creation.
singleton :: Ord v => ROBDDContext v -> v -> (ROBDDContext v, ROBDD v)
singleton context var = mkNode context Zero var One

-- | Creates a new singleton from a variable. The corresponding formula evaluates to True when the
-- variable is False and vice-versa. This is the same as 'execState . notC . singleton' If the same
-- singleton already exists and the context, no node is created and an ROBDDRef is returned
-- instead. Use 'singletonNotC' for an implicitly contextualized node creation.
singletonNot :: Ord v => ROBDDContext v -> v -> (ROBDDContext v, ROBDD v)
singletonNot context var = mkNode context One var Zero
