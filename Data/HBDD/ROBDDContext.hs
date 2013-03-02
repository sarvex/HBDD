module Data.HBDD.ROBDDContext
(
ROBDDId
, ROBDDContext
, mkContext
)
where

import Data.HBDD.ROBDD
import Data.HBDD.UIDGenerator
import qualified Data.Map as M

type ROBDDId v = (UID, v, UID)
data ROBDDContext v = ROBDDContext UIDGenerator (M.Map (ROBDDId v) (ROBDD v))

mkContext :: ROBDDContext v
mkContext = ROBDDContext mkGenerator M.empty
