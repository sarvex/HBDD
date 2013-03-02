module Data.HBDD.UIDGenerator
(
UID
, UIDGenerator
, mkGenerator
, allocId
)
where

type UID = Int

newtype UIDGenerator = UIDGenerator [ UID ]
                       deriving Show

mkGenerator :: UIDGenerator
mkGenerator = UIDGenerator [ 2 .. ]

allocId :: UIDGenerator -> (Int, UIDGenerator)
allocId (UIDGenerator [])      = error "Infinite list cannot be empty."
allocId (UIDGenerator (i:ids)) = (i, UIDGenerator ids)
