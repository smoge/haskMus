{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Music.Time.Rtm where

import           Data.List          (foldl')
import Test.QuickCheck
    ( Gen,
      Arbitrary(arbitrary),
      choose,
      oneof,
      resize,
      sized,
      vectorOf )
import           Text.Pretty.Simple

data RtmValue
  = RtmNote Int
  | RtmRest Int
  | RtmLeaf Int RtmProportions
  deriving (Eq, Ord, Show)

data RtmProportions =
  RtmProportions [RtmValue]
  deriving (Eq, Ord, Show)

data RtmStructure
  = RtmScalar
  | RtmVector Int [RtmStructure]
  deriving (Eq, Ord, Show)

data ArrayShape
  = Scalar
  | VectorP [ArrayShape] -- Represents RtmProportions
  | VectorL [ArrayShape] -- Represents RtmLeaf
  deriving (Eq, Show)

-- ! FIXME: Maybe it's better the other way, but something is buggy here
data RtmArray =
  RtmArray [Int] ArrayShape
  deriving (Eq, Show)

toRtmArray :: RtmProportions -> RtmArray
toRtmArray (RtmProportions values) =
  let (flattenedValues, shape) = flattenRtmValues values
   in RtmArray flattenedValues shape

flattenRtmValues :: [RtmValue] -> ([Int], ArrayShape)
flattenRtmValues values =
  let (ints, shapes) = unzip (map flattenValue values)
   in (concat ints, VectorP shapes) -- Updated Vector constructor to VectorP

flattenValue :: RtmValue -> ([Int], ArrayShape)
flattenValue (RtmNote n) = ([n], Scalar)
flattenValue (RtmRest r) = ([-r], Scalar) -- Negative for rests
flattenValue (RtmLeaf n (RtmProportions props)) =
  let (flattenedValues, shape) = flattenRtmValues props
   in (n : flattenedValues, VectorL [Scalar, shape])

extractValuesAndShape :: RtmProportions -> ([Int], ArrayShape)
extractValuesAndShape (RtmProportions values) =
  let (flatValues, shapes) = unzip (map extractFromRtmValue values)
   in (concat flatValues, VectorP shapes) -- Updated Vector constructor to VectorP

extractFromRtmValue :: RtmValue -> ([Int], ArrayShape)
extractFromRtmValue (RtmNote n) = ([n], Scalar)
extractFromRtmValue (RtmRest n) = ([n], Scalar)
extractFromRtmValue (RtmLeaf n props) =
  let (vals, shape) = extractValuesAndShape props
   in (n : vals, VectorL [Scalar, shape])

-- ! FIXME
-- -- | RtmArray to RtmProportions
fromRtmArray :: RtmArray -> RtmProportions
fromRtmArray (RtmArray values shape) =
  let (rtmValues, _) = reconstructRtmValues values shape
   in RtmProportions rtmValues

reconstructRtmValues :: [Int] -> ArrayShape -> ([RtmValue], [Int])
reconstructRtmValues vals (VectorP shapes) =
  let (values, rest) =
        foldl'
          (\(acc, remaining) shp ->
             let (v, r) = reconstructValue remaining shp
              in (acc ++ v, r))
          ([], vals)
          shapes
   in (values, rest)
reconstructRtmValues xs Scalar =
  let (values, rest) =
        foldr
          (\x (accVals, accRest) ->
             let (v, r) = reconstructValue [x] Scalar
              in (v ++ accVals, r ++ accRest))
          ([], [])
          xs
   in (values, rest)

reconstructValue :: [Int] -> ArrayShape -> ([RtmValue], [Int])
reconstructValue (n:xs) (VectorP (shp:shps))
  | n >= 0 =
    let (values, rest) = reconstructRtmValues (take n xs) shp
     in ( [RtmLeaf n (RtmProportions values)]
        , drop n xs ++ reconstructRemainder rest shps)
  | otherwise =
    let (values, rest) = reconstructRtmValues (take (-n) xs) shp
     in ([RtmLeaf (-n) (RtmProportions values)], drop (-n) xs ++ rest)
reconstructValue (n:xs) (VectorL (shp:shps))
  | n >= 0 =
    let (values, rest) = reconstructRtmValues (take n xs) shp
     in ( [RtmLeaf n (RtmProportions values)]
        , drop n xs ++ reconstructRemainder rest shps)
  | otherwise =
    let (values, rest) = reconstructRtmValues (take (-n) xs) shp
     in ([RtmLeaf (-n) (RtmProportions values)], drop (-n) xs ++ rest)
reconstructValue (n:xs) Scalar
  | n >= 0 = ([RtmNote n], xs)
  | otherwise = ([RtmRest (-n)], xs)
reconstructValue [] _ = error "Unexpected empty list"
reconstructValue (_:_) (VectorP []) =
  error "Unexpected VectorP shape with no sub-shapes"
reconstructValue (_:_) (VectorL []) =
  error "Unexpected VectorL shape with no sub-shapes"

reconstructRemainder :: [Int] -> [ArrayShape] -> [Int]
reconstructRemainder vals [] = vals
reconstructRemainder vals (shp:shps) =
  let (_, rest) = reconstructRtmValues vals shp
   in reconstructRemainder rest shps

structureOfRtm' :: RtmProportions -> [RtmStructure]
structureOfRtm' (RtmProportions values) = map structureOfRtm values

countRtmProportions :: RtmProportions -> Int
countRtmProportions (RtmProportions values) = length values

structureOfRtm :: RtmValue -> RtmStructure
structureOfRtm (RtmNote _) = RtmScalar
structureOfRtm (RtmRest _) = RtmScalar
structureOfRtm (RtmLeaf _ proportions) =
  RtmVector (countRtmProportions proportions) (structureOfRtm' proportions)

shapeOfRtmProportions :: RtmProportions -> [Int]
shapeOfRtmProportions (RtmProportions values) =
  length values : mergeShapes (map shapeOfRtm values)

shapeOfRtm :: RtmValue -> [Int]
shapeOfRtm (RtmNote _)             = []
shapeOfRtm (RtmRest _)             = []
shapeOfRtm (RtmLeaf _ proportions) = 1 : shapeOfRtmProportions proportions

mergeShapes :: [[Int]] -> [Int]
mergeShapes = foldr zipWithMax []
  where
    zipWithMax xs ys = zipWith max xs (ys ++ repeat 0) ++ drop (length xs) ys

leafRanks :: RtmValue -> [(RtmValue, Int)]
leafRanks val = leafRanksHelper val 0

leafRanksHelper :: RtmValue -> Int -> [(RtmValue, Int)]
leafRanksHelper (RtmLeaf _ (RtmProportions rtmVals)) depth =
  concatMap (\v -> leafRanksHelper v (depth + 1)) rtmVals
leafRanksHelper (RtmNote n) depth = [(RtmNote n, depth)]
leafRanksHelper (RtmRest n) depth = [(RtmRest n, depth)]

leafRanksFromProportions :: RtmProportions -> [(RtmValue, Int)]
leafRanksFromProportions (RtmProportions rtmVals) =
  leafRanksHelperForProportions rtmVals 0

leafRanksHelperForProportions :: [RtmValue] -> Int -> [(RtmValue, Int)]
leafRanksHelperForProportions rtmVals depth =
  concatMap (\v -> leafRanksHelper' v (depth + 1)) rtmVals

leafRanksHelper' :: RtmValue -> Int -> [(RtmValue, Int)]
leafRanksHelper' (RtmLeaf _ (RtmProportions rtmVals)) depth =
  concatMap (\v -> leafRanksHelper' v (depth + 1)) rtmVals
leafRanksHelper' (RtmNote n) depth = [(RtmNote n, depth)]
leafRanksHelper' (RtmRest n) depth = [(RtmRest n, depth)]

data Path = Path
  { indices :: [Int]
  , value   :: RtmValue
  } deriving (Show)

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

aplFilter :: (RtmValue -> Bool) -> RtmProportions -> RtmProportions
aplFilter p (RtmProportions values) = RtmProportions (filterValues p values)

filterValues :: (RtmValue -> Bool) -> [RtmValue] -> [RtmValue]
filterValues _ [] = []
filterValues p (v:vs)
  | p v = v : filterValues p vs
  | otherwise = filterValues p vs

aplReduce ::
     (RtmValue -> RtmValue -> RtmValue) -> RtmProportions -> RtmProportions
aplReduce f (RtmProportions values) = RtmProportions (reduceValues f values)

-- aplReduce _ x = x
reduceValues :: (RtmValue -> RtmValue -> RtmValue) -> [RtmValue] -> [RtmValue]
reduceValues _ []         = []
reduceValues _ [x]        = [x]
reduceValues f (x:y:rest) = reduceValues f (f x y : reduceValues f rest)

-- reduceValues _ xs = xs  -- Handle the cases for RtmNote and RtmRest
sumList :: [RtmValue] -> RtmValue
sumList values =
  case reduceValues (\x y -> RtmNote (getValue' x + getValue' y)) values of
    [result] -> result
    _        -> RtmRest 0

-- Define a function to extract the value from RtmValue
getValue' :: RtmValue -> Int
getValue' (RtmNote n) = n
getValue' _           = 0

aplMap :: (RtmValue -> RtmValue) -> RtmProportions -> RtmProportions
aplMap f (RtmProportions values) = RtmProportions (map (aplMapValue f) values)

aplMapValue :: (RtmValue -> RtmValue) -> RtmValue -> RtmValue
aplMapValue f (RtmLeaf x proportions) = RtmLeaf x (aplMap f proportions)
aplMapValue f value                   = f value

-- Define a function to transpose a musical note by adding 2 to its pitch
transposeNote :: RtmValue -> RtmValue
transposeNote (RtmNote pitch) = RtmNote (pitch + 2)
transposeNote value           = value

-- Define a function to sum two musical rests
combineRests :: RtmValue -> RtmValue -> RtmValue
combineRests (RtmRest pitch1) (RtmRest pitch2) = RtmRest (pitch1 + pitch2)
combineRests value1 _                          = value1

-- Reduce an RtmProportions by combining its elements
combineProportions :: RtmProportions -> RtmProportions
combineProportions (RtmProportions values) =
  RtmProportions (combineValues values)

combineValues :: [RtmValue] -> [RtmValue]
combineValues [] = []
combineValues [x] = [x]
combineValues (x:y:rest)
  | isCombinable x && isCombinable y = combineRests x y : combineValues rest
  | otherwise = x : combineValues (y : rest)
  where
    isCombinable (RtmRest _) = True
    isCombinable _           = False

-- # SECTION QuickCheck
maxDepth :: Int
maxDepth = 2 -- Adjust the maxDepth as needed

instance Arbitrary RtmValue where
  arbitrary = sized arbRtmValue

arbRtmValue :: Int -> Gen RtmValue
arbRtmValue n
  | n <= 0 = oneof [RtmNote <$> choose (1, 4), RtmRest <$> choose (1, 4)]
  | n > maxDepth = oneof [RtmNote <$> choose (4, 9), RtmRest <$> choose (4, 9)]
  | otherwise =
    oneof
      [ RtmNote <$> choose (10, 14)
      , RtmRest <$> choose (10, 14)
      , RtmLeaf <$> choose (10, 14) <*> resize (n - 1) arbitrary
      ]

instance Arbitrary RtmProportions where
  arbitrary = do
    depth <- choose (1, maxDepth)
    generateRtmProportions depth
    where
      generateRtmProportions :: Int -> Gen RtmProportions
      generateRtmProportions 1 = do
        numChildren <- choose (2, 4)
        rtmValues <- vectorOf numChildren arbitrary
        return (RtmProportions rtmValues)
      generateRtmProportions depth = do
        rtmValue <- arbitrary
        -- Ensure at least three children when depth allows for it
        numChildren <- choose (2, 4) -- Adjust the number of children as needed
        children <- vectorOf numChildren (generateRtmProportions (depth - 1))
        return
          (RtmProportions
             (rtmValue : concatMap (\(RtmProportions xs) -> xs) children))

genRtmProportions :: Gen RtmProportions
genRtmProportions = do
  numElements <- choose (2, 4)
  rtmValues <- vectorOf numElements genRtmValue
  return (RtmProportions rtmValues)

-- Define a custom generator for RtmValue
genRtmValue :: Gen RtmValue
genRtmValue =
  oneof
    [ RtmNote <$> arbitrary
    , RtmRest <$> arbitrary
      -- You can add more cases for other RtmValue constructors
    ]
