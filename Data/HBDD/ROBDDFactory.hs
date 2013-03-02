module Data.HBDD.ROBDDFactory
(
mkNode
, singleton
)
where

import Prelude hiding(lookup)
import Data.HBDD.ROBDD
import Data.HBDD.ROBDDContext

mkNode :: Ord v => ROBDDContext v -> ROBDD v -> v -> ROBDD v -> (ROBDDContext v, ROBDD v)
mkNode context l v r = case lookup (idl, v, idr) context of
                       Just c  -> (context, ROBDDRef idl v idr $ identifier c)
                       Nothing -> let (uid, ctx) = allocId context
                                      res        = ROBDD l v r uid
                                      ctx'       = insert (idl, v, idr) res ctx
                                  in
                                  (ctx', res)

                       where
                       idl = identifier l
                       idr = identifier r

singleton :: Ord v => ROBDDContext v -> v -> (ROBDDContext v, ROBDD v)
singleton context var = mkNode context Zero var One
