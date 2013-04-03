import Data.HBDD.Operations
import Control.Monad.State.Strict
import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Data.List
import Data.Either
import Prelude hiding(and,or,not)
import qualified Data.Map as M

-- | USA graph coloring, but with fewer states (to make computation times more acceptable).
main :: IO ()
main = let (numToState, bdd) = usa in
       let (bdd', context)   = runState bdd mkContext in
       case getSat context bdd' >>= (return . outputColors numToState) of
       Nothing -> return ()
       Just s  -> putStrLn s

-- | Outputs the color of a state on the format "STATE Color\n\r"
outputColors :: M.Map Int String -> [Either Int Int] -> String
outputColors numToState result = foldr1 (++) $ map numberToState goodColors
                                 where
                                 goodColors    = rights result
                                 numToColor    = M.fromList $ [(0, "red"),
                                                               (1, "green"),
                                                               (2, "blue"),
                                                               (3, "yellow")]
                                 numberToState num = let nameId  = (num `div` 4) * 4
                                                         colorId = num `mod` 4
                                                     in
                                                     M.findWithDefault undefined nameId numToState
                                                     ++ " "
                                                     ++ M.findWithDefault undefined colorId numToColor
                                                     ++ "\n\r"

-- | Restriction saying that the neighbourgs of a node cannot have the same color
edge :: Int -> Int -> ROBDDState Int
edge state1 state2 = (state1Color1 .=>. notState2Color1)
                     .&. (state1Color2 .=>. notState2Color2)
                     .&. (state1Color3 .=>. notState2Color3)
                     .&. (state1Color4 .=>. notState2Color4)
                     where
                     state1Color1 = singletonC $ state1 + 0 -- r
                     state1Color2 = singletonC $ state1 + 1 -- g
                     state1Color3 = singletonC $ state1 + 2 -- b
                     state1Color4 = singletonC $ state1 + 3 -- y
                     notState2Color1 = singletonNotC $ state2 + 0 -- r
                     notState2Color2 = singletonNotC $ state2 + 1 -- g
                     notState2Color3 = singletonNotC $ state2 + 2 -- b
                     notState2Color4 = singletonNotC $ state2 + 3 -- y

-- | Restrictions saying that a state has only one color
oneColor :: Int -> ROBDDState Int
oneColor statec = foldr1 (.|.) $ map forbid colors
                  where
                  forbid i = singletonC i
                             .&. foldr1 (.&.) [ singletonNotC c | c <- colors, c /= i ]
                  color1 = statec + 0 -- r
                  color2 = statec + 1 -- g
                  color3 = statec + 2 -- b
                  color4 = statec + 3 -- y
                  colors = [ color1, color2, color3, color4 ]

-- | BDD of the USA graph-coloring problem.
usa :: (M.Map Int String, ROBDDState Int)
usa = (toName, (foldr1 (.&.) $ map oneColor allStates)
      .&. edge az ca  .&. edge az nv  .&. edge ca nv .&. edge ca or_ .&. edge id_ mt
      .&. edge id_ nv .&. edge id_ or_ .&. edge id_ wa .&. edge nv or_ .&. edge or_ wa)
      where
      allStates = [az, ca, id_, mt, nv, or_, wa]
      toName = M.fromList $ [(az, "AZ"), (ca, "CA"), (id_, "ID"), (mt, "MT"), (nv, "NV") 
                             , (or_, "OR"), (wa, "WA") ]
      az  = 3 * 4 :: Int
      ca  = 5 * 4 :: Int
      id_ = 12 * 4 :: Int
      mt  = 26 * 4 :: Int
      nv  = 28 * 4 :: Int
      or_ = 37 * 4 :: Int
      wa  = 47 * 4 :: Int
