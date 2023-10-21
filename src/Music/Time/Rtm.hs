{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Music.Time.Rtm where

import Data.List (foldl')

data RtmValue
  = RtmNote Int
  | RtmRest Int
  | RtmLeaf Int RtmProportions
  deriving (Eq, Ord, Show)

data RtmProportions = RtmProportions [RtmValue]
  deriving (Eq, Ord, Show)

data RtmStructure
  = RtmScalar
  | RtmVector Int [RtmStructure]
  deriving (Eq, Ord, Show)

{- | RtmProportion to RtmStructure
>>> tree2 = RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]
>>> structureOfRtm' tree2 
[RtmScalar,RtmVector 2 [RtmScalar,RtmScalar],RtmScalar]
-}

data ArrayShape
  = Scalar
  | Vector [ArrayShape]
  deriving (Eq, Ord, Show)

data RtmArray = RtmArray [Int] ArrayShape
  deriving (Eq, Ord, Show)

{- | RtmProportion to RtmArray
>>> rtm = RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]
>>> toRtmArray rtm
RtmArray [5,2,6,-4,-3] (Vector [Scalar,Vector [Scalar,Vector [Scalar,Scalar]],Scalar])

>>> array = toRtmArray rtm
>>> fromRtmArray array
RtmProportions [RtmNote 5,RtmLeaf 2 (RtmProportions [RtmNote 6,RtmRest 4]),RtmRest 3]

>>>  rtm == (fromRtmArray . toRtmArray) rtm
True

-}
-- toRtmArray :: RtmProportions -> RtmArray
-- toRtmArray props = let (vals, shape) = extractValuesAndShape props in RtmArray vals shape

toRtmArray :: RtmProportions -> RtmArray
toRtmArray (RtmProportions values) =
  let (flattenedValues, shape) = flattenRtmValues values
  in RtmArray flattenedValues shape

flattenRtmValues :: [RtmValue] -> ([Int], ArrayShape)
flattenRtmValues values =
  let (ints, shapes) = unzip (map flattenValue values)
  in (concat ints, Vector shapes)

flattenValue :: RtmValue -> ([Int], ArrayShape)
flattenValue (RtmNote n) = ([n], Scalar)
flattenValue (RtmRest r) = ([-r], Scalar)  -- Negative for rests
flattenValue (RtmLeaf n (RtmProportions props)) =
  let (flattenedValues, shape) = flattenRtmValues props
  in (n : flattenedValues, Vector [Scalar, shape])


extractValuesAndShape :: RtmProportions -> ([Int], ArrayShape)
extractValuesAndShape (RtmProportions values) =
  let (flatValues, shapes) = unzip (map extractFromRtmValue values)
  in (concat flatValues, Vector shapes)

extractFromRtmValue :: RtmValue -> ([Int], ArrayShape)
extractFromRtmValue (RtmNote n) = ([n], Scalar)
extractFromRtmValue (RtmRest n) = ([n], Scalar)
extractFromRtmValue (RtmLeaf n props) =
  let (vals, shape) = extractValuesAndShape props
  in (n:vals, Vector [Scalar, shape])

-- | RtmArray to RtmProportions

fromRtmArray :: RtmArray -> RtmProportions
fromRtmArray (RtmArray values shape) =
  let (rtmValues, _) = reconstructRtmValues values shape
  in RtmProportions rtmValues

reconstructRtmValues :: [Int] -> ArrayShape -> ([RtmValue], [Int])
reconstructRtmValues vals (Vector shapes) = 
  let (values, rest) = foldl' (\(acc, remaining) shp -> 
          let (v, r) = reconstructValue remaining shp 
          in (acc ++ v, r)) ([], vals) shapes
  in (values, rest)
reconstructRtmValues xs Scalar = 
  let (values, rest) = foldr (\x (accVals, accRest) -> 
                                let (v, r) = reconstructValue [x] Scalar 
                                in (v ++ accVals, r ++ accRest)
                            ) ([], []) xs
  in (values, rest)

reconstructValue :: [Int] -> ArrayShape -> ([RtmValue], [Int])
reconstructValue (n:xs) (Vector (shp:shps)) 
  | n >= 0 = 
      let (values, rest) = reconstructRtmValues (take n xs) shp
      in ([RtmLeaf n (RtmProportions values)], drop n xs ++ reconstructRemainder rest shps)
  | otherwise = 
      let (values, rest) = reconstructRtmValues (take (-n) xs) shp
      in ([RtmLeaf (-n) (RtmProportions values)], drop (-n) xs ++ reconstructRemainder rest shps)
reconstructValue (n:xs) Scalar
  | n >= 0 = ([RtmNote n], xs)
  | otherwise = ([RtmRest (-n)], xs)
reconstructValue [] _ = error "Unexpected empty list"
reconstructValue (_:_) (Vector []) = error "Unexpected Vector shape with no sub-shapes"

-- Helper function to handle the remainder of the shapes
reconstructRemainder :: [Int] -> [ArrayShape] -> [Int]
reconstructRemainder vals [] = vals
reconstructRemainder vals (shp:shps) = 
    let (_, rest) = reconstructRtmValues vals shp
    in reconstructRemainder rest shps


{-

RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 3])]
-- This would flatten to:
[2, 6, -3], Vector [Scalar, Scalar]


size = 5
shape = [Int, Int [Int,Int],Int]

RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]

-- ?? 
Array [5, 2, 6, 4, 3]

tree2 = RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]

RtmArray [5,2,6,4,3] (Vector [Scalar,Vector [Scalar,Vector [Scalar,Scalar]],Scalar])


-}
structureOfRtm' :: RtmProportions -> [RtmStructure]
structureOfRtm' (RtmProportions values) = map structureOfRtm values

countRtmProportions :: RtmProportions -> Int
countRtmProportions (RtmProportions values) = length values

{- | RtmValue to RtmStructure
>>> tree1 = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3])
>>> structureOfRtm tree1 == RtmVector 3 [RtmScalar,RtmVector 2 [RtmScalar,RtmScalar],RtmScalar]
True

>>> tree3 = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4, RtmLeaf 3 (RtmProportions [RtmRest 2])]), RtmRest 3])
>>> structureOfRtm tree3 == RtmVector 3 [RtmScalar,RtmVector 3 [RtmScalar,RtmScalar,RtmVector 1 [RtmScalar]],RtmScalar]
True
-}
structureOfRtm :: RtmValue -> RtmStructure
structureOfRtm (RtmNote _) = RtmScalar
structureOfRtm (RtmRest _) = RtmScalar
structureOfRtm (RtmLeaf _ proportions) = RtmVector (countRtmProportions proportions) (structureOfRtm' proportions)


shapeOfRtmProportions :: RtmProportions -> [Int]
shapeOfRtmProportions (RtmProportions values) =
  length values : mergeShapes (map shapeOfRtm values)

{- | 
>>> tree5 = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3])
>>> shapeOfRtm tree5 == [1,3,1,2]
True

>>> tree_test = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3])
>>> shapeOfRtm tree_test ==  [1,3,1,2]
True

>>> tree2 = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4, RtmLeaf 3 (RtmProportions [RtmRest 2])]), RtmRest 3])
>>> shapeOfRtm tree2 == [1,3,1,3,1,1]
True

>>> tree3 = RtmLeaf 4 (RtmProportions [RtmRest 3])
>>> shapeOfRtm tree3 == [1,1]
True
-}
shapeOfRtm :: RtmValue -> [Int]
shapeOfRtm (RtmNote _) = []
shapeOfRtm (RtmRest _) = []
shapeOfRtm (RtmLeaf _ proportions) = 1 : shapeOfRtmProportions proportions



mergeShapes :: [[Int]] -> [Int]
mergeShapes = foldr zipWithMax []
  where
    zipWithMax xs ys = zipWith max xs (ys ++ repeat 0) ++ drop (length xs) ys

{- |
>>> tree1 = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3])
>>> tree2 = RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]
>>> leafRanks tree1 == [(RtmNote 5,1),(RtmNote 6,2),(RtmRest 4,2),(RtmRest 3,1)]
>>> leafRanksFromProportions tree2 == [(RtmNote 5,1),(RtmNote 6,2),(RtmRest 4,2),(RtmRest 3,1)]
True
True

>>> mergeShapes [[1, 2], [1, 2, 3], [1]] == [1,2,3]
>>> mergeShapes [[1, 2, 3], [1], [1, 2]] == [1,2,3]
>>> mergeShapes [[1, 2], [1, 2], [1, 2]] == [1,2]
True
True
True
 -}
leafRanks :: RtmValue -> [(RtmValue, Int)]
leafRanks val = leafRanksHelper val 0

leafRanksHelper :: RtmValue -> Int -> [(RtmValue, Int)]
leafRanksHelper (RtmLeaf _ (RtmProportions rtmVals)) depth = concatMap (\v -> leafRanksHelper v (depth + 1)) rtmVals
leafRanksHelper (RtmNote n) depth = [(RtmNote n, depth)]
leafRanksHelper (RtmRest n) depth = [(RtmRest n, depth)]

leafRanksFromProportions :: RtmProportions -> [(RtmValue, Int)]
leafRanksFromProportions (RtmProportions rtmVals) = leafRanksHelperForProportions rtmVals 0

leafRanksHelperForProportions :: [RtmValue] -> Int -> [(RtmValue, Int)]
leafRanksHelperForProportions rtmVals depth = concatMap (\v -> leafRanksHelper' v (depth + 1)) rtmVals

leafRanksHelper' :: RtmValue -> Int -> [(RtmValue, Int)]
leafRanksHelper' (RtmLeaf _ (RtmProportions rtmVals)) depth = concatMap (\v -> leafRanksHelper' v (depth + 1)) rtmVals
leafRanksHelper' (RtmNote n) depth = [(RtmNote n, depth)]
leafRanksHelper' (RtmRest n) depth = [(RtmRest n, depth)]



data Path = Path {indices :: [Int], value :: RtmValue}
  deriving (Show)

leafPaths :: RtmValue -> [(RtmValue, [Int])]
leafPaths val = leafPathsHelper val []

leafPathsHelper :: RtmValue -> [Int] -> [(RtmValue, [Int])]
leafPathsHelper (RtmLeaf _ (RtmProportions rtmVals)) path =
  concatMap (\(idx, v) -> leafPathsHelper v (idx : path)) (zip [0 ..] rtmVals)
leafPathsHelper (RtmNote n) path = [(RtmNote n, reverse path)]
leafPathsHelper (RtmRest n) path = [(RtmRest n, reverse path)]

leafPaths' :: RtmValue -> [[Int]]
leafPaths' val = leafPathsHelper' val []

leafPathsHelper' :: RtmValue -> [Int] -> [[Int]]
leafPathsHelper' (RtmLeaf _ (RtmProportions rtmVals)) path =
  concatMap (\(idx, v) -> leafPathsHelper' v (idx : path)) (zip [0 ..] rtmVals)
leafPathsHelper' (RtmNote n) path = [reverse path]
leafPathsHelper' (RtmRest n) path = [reverse path]

-- Function to get the lengths of paths
pathLengths :: RtmValue -> [Int]
pathLengths val = map length (leafPaths' val)

{-
>>> tree1 = RtmLeaf 1 (RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3])
>>> tree2 = RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]
>>> leafPaths tree1
[(RtmNote 5,[0]),(RtmNote 6,[1,0]),(RtmRest 4,[1,1]),(RtmRest 3,[2])]

>>> leafPaths' tree1
[[0],[1,0],[1,1],[2]]

>>> map length (leafPaths' tree1)
[1,2,2,1]

>>> pathLengths tree1
[1,2,2,1]
 -}

aplFilter :: (RtmValue -> Bool) -> RtmProportions -> RtmProportions
aplFilter p (RtmProportions values) = RtmProportions (filterValues p values)

filterValues :: (RtmValue -> Bool) -> [RtmValue] -> [RtmValue]
filterValues _ [] = []
filterValues p (v : vs)
  | p v = v : filterValues p vs
  | otherwise = filterValues p vs

{-
tree4 = RtmProportions [RtmNote 5, RtmLeaf 2 (RtmProportions [RtmNote 6, RtmRest 4]), RtmRest 3]
aplFilter (\x -> case x of RtmNote n -> n `mod` 2 == 0; _ -> True) tree4 -- Filter even RtmNotes
-- RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 6,RtmRest 4]),RtmRest 3]
-}

aplReduce :: (RtmValue -> RtmValue -> RtmValue) -> RtmProportions -> RtmProportions
aplReduce f (RtmProportions values) = RtmProportions (reduceValues f values)

-- aplReduce _ x = x

reduceValues :: (RtmValue -> RtmValue -> RtmValue) -> [RtmValue] -> [RtmValue]
reduceValues _ [] = []
reduceValues _ [x] = [x]
reduceValues f (x : y : rest) = reduceValues f (f x y : reduceValues f rest)

-- reduceValues _ xs = xs  -- Handle the cases for RtmNote and RtmRest

sumList :: [RtmValue] -> RtmValue
sumList values = case reduceValues (\x y -> RtmNote (getValue' x + getValue' y)) values of
  [result] -> result
  _ -> RtmRest 0

-- Define a function to extract the value from RtmValue
getValue' :: RtmValue -> Int
getValue' (RtmNote n) = n
getValue' _ = 0

{-
inputList = [RtmNote 1, RtmNote 2, RtmNote 3]
sumList inputList
-- RtmNote 6
-}

{-
 -- Summing all RtmNotes in a sequence
input1 = RtmProportions [RtmNote 5, RtmNote 3, RtmNote 2]
aplReduce (\x y -> RtmNote (getValue x + getValue y)) input1
-- Expected result: RtmProportions [RtmNote 10]
-- Got: RtmProportions [RtmNote 10]

  -}

aplMap :: (RtmValue -> RtmValue) -> RtmProportions -> RtmProportions
aplMap f (RtmProportions values) = RtmProportions (map (aplMapValue f) values)

aplMapValue :: (RtmValue -> RtmValue) -> RtmValue -> RtmValue
aplMapValue f (RtmLeaf x proportions) = RtmLeaf x (aplMap f proportions)
aplMapValue f value = f value

-- Define a function to transpose a musical note by adding 2 to its pitch
transposeNote :: RtmValue -> RtmValue
transposeNote (RtmNote pitch) = RtmNote (pitch + 2)
transposeNote value = value

-- Define a function to sum two musical rests
combineRests :: RtmValue -> RtmValue -> RtmValue
combineRests (RtmRest pitch1) (RtmRest pitch2) = RtmRest (pitch1 + pitch2)
combineRests value1 _ = value1

-- Reduce an RtmProportions by combining its elements
combineProportions :: RtmProportions -> RtmProportions
combineProportions (RtmProportions values) = RtmProportions (combineValues values)

combineValues :: [RtmValue] -> [RtmValue]
combineValues [] = []
combineValues [x] = [x]
combineValues (x : y : rest)
  | isCombinable x && isCombinable y = combineRests x y : combineValues rest
  | otherwise = x : combineValues (y : rest)
  where
    isCombinable (RtmRest _) = True
    isCombinable _ = False

{-
inputProportions = RtmProportions [RtmNote 5, RtmNote 3, RtmRest 3, RtmRest 2, RtmNote 2]
combineProportions inputProportions

-- RtmProportions [RtmNote 5,RtmNote 3,RtmRest 5,RtmNote 2]

 -}
