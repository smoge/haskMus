{-# LANGUAGE DeriveFunctor #-}

module Rtm.Shape where

import Data.Tree
import Rtm.Common
import System.Random.Shuffle (shuffleM)

-- | Tree structure that holds placeholders for the shape of the tree
data ShapeTree a = ShapeLeaf | ShapeNode a [ShapeTree a]
  deriving (Functor, Show)

-- Function to extract the shape of an Rtm
extractShape :: Rtm -> (ShapeTree (), [RtmLabel])
extractShape = go
  where
    go (Node label children) =
      let (childShapes, childValues) = unzip (fmap go children)
       in (ShapeNode () childShapes, label : concat childValues)

-- | Reconstruct an Rtm from its shape and a list of values
reconstruct :: ShapeTree () -> [RtmLabel] -> Rtm
reconstruct sh vs = fst $ rHelper sh vs

rHelper :: ShapeTree () -> [RtmLabel] -> (Rtm, [RtmLabel])
rHelper = go
  where
    go (ShapeNode _ cs) (val : vs) =
      let (children, remainingVs) = foldl goChildren ([], vs) cs
       in (Node val children, remainingVs)
    go ShapeLeaf vs = (Node (head vs) [], tail vs)
    go (ShapeNode _ _) [] = error "Not enough values to match the shape"

    goChildren (cs, vs) z =
      let (child, newVs) = go z vs
       in (cs <> [child], newVs)

-- ====== TESTS ======= --

-- | An example Rtm.
example :: Rtm
example = rtm' [s 1, g 3, 2 |: [s 1, g 1, s 1], s 1]

shape :: ShapeTree ()
values :: [RtmLabel]
(shape, values) = extractShape example

reconstructedRtm :: Rtm
reconstructedRtm = reconstruct shape values

intList :: [Int]
intList = extractIntsFromLabels values

p :: IO [Int]
p = shuffleM intList

-- reconstructLabelsFromInts:

-- Sample original RtmLabel list
originalLabels :: [RtmLabel]
originalLabels = [RtmCons, RtmScalar 1, RtmGap 2, RtmVector 3, RtmScalar 4, RtmScalar 5]

-- New integers to insert into the RtmLabel list
newInts :: [Int]
newInts = [10, 20, 30, 40, 50]

-- Perform the reconstruction
reconstructedLabels :: [RtmLabel]
reconstructedLabels = reconstructLabelsFromInts newInts originalLabels

-- [RtmCons,RtmScalar 10,RtmGap 20,RtmVector 30,RtmScalar 40,RtmScalar 50]

{- ---------------------------------------------------------------------------------------------
(shape, values) =  extractShape example

shape
-- ShapeNode () [ShapeNode () [],ShapeNode () [],ShapeNode () [ShapeNode () [],ShapeNode () [],ShapeNode () []],ShapeNode () []]

values
-- [RtmCons,RtmScalar 1,RtmGap 3,RtmVector 2,RtmScalar 1,RtmGap 1,RtmScalar 1,RtmScalar 1]

reconstructedRtm = reconstruct shape values

extractIntsFromLabels values
-- [1,3,2,1,1,1,1]

shuffleM $ extractIntsFromLabels values

isValidRtm reconstructedRtm
-- True

showRtm example
-- "(1 -3 (2 (1 -1 1)) 1)"
showRtm reconstructedRtm
-- "(1 -3 (2 (1 -1 1)) 1)"

--------------------------------------------------------------------------------------------- -}
