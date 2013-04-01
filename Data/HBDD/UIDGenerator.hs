module Data.HBDD.UIDGenerator
(
UID
, UIDGenerator
, mkGenerator
, allocId
)
where

-- | Identifier type.
type UID = Int

-- | A generator of identifier.
newtype UIDGenerator = UIDGenerator [ UID ]
                       deriving Show

-- | Creates a new generator.
mkGenerator :: UIDGenerator
mkGenerator = UIDGenerator [ 2 .. ]

-- | Allocates a new identifier and returns a new generator unable to reallocate the same
-- identifier again.
allocId :: UIDGenerator -> (Int, UIDGenerator)
allocId (UIDGenerator [])      = error "Infinite list cannot be empty."
allocId (UIDGenerator (i:ids)) = (i, UIDGenerator ids)
