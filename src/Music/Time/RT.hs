{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Music.Time.RT where

import           Control.Monad      (foldM)
import           Data.Bits          ((.&.))
import           Data.Ratio
import           Test.QuickCheck
import           Text.Pretty.Simple

type Dur = Rational -- ^ Duration

data Component
  = Scalar Int
  | Gap Int
  | Vector Int [Component]
  deriving (Eq, Ord, Show)

-- | This function checks if the Component is valid.
--
-- >>> isValidComponent $ Scalar 5
-- True
-- >>> isValidComponent $ Gap 3
-- True
-- >>> isValidComponent $ Vector 4 []
-- False
-- >>> isValidComponent $ Vector 4 [Scalar 2]
-- False
-- >>> isValidComponent $ Vector 4 [Scalar 2, Gap 3]
-- True
-- >>> isValidComponent $ Vector (-1) [Scalar 2, Gap 3]
-- False
isValidComponent :: Component -> Bool
isValidComponent (Scalar n) = n >= 0
isValidComponent (Gap n) = n >= 0
isValidComponent (Vector n xs)
  | n < 0 = False
  | null xs = False
  | length xs == 1 = False
  | otherwise = all isValidComponent xs

-- | This function simplifies an Component.
--
-- >>> simplifyComponent $ Scalar 5
-- Scalar 5
-- >>> simplifyComponent (Gap 3)
-- Gap 3
-- >>> simplifyComponent (Vector 4 [])
-- Scalar 4
-- >>> simplifyComponent (Vector 4 [Scalar 2])
-- Scalar 4
-- >>> simplifyComponent (Vector 4 [Scalar 2, Gap 3])
-- Vector 4 [Scalar 2,Gap 3]
-- >>> simplifyComponent (Vector 4 [Gap 1, Gap 2, Gap 3, Scalar 2])
-- Vector 4 [Gap 6,Scalar 2]
simplifyComponent :: Component -> Component
simplifyComponent r@(Scalar _) = r
simplifyComponent r@(Gap _) = r
simplifyComponent (Vector n [])
  | n == 0 = Gap 0
  | n >= 0 = Scalar n
  | n < 0 = Gap (negate n)
simplifyComponent (Vector n [x]) = simplifyComponent (Vector n [])
simplifyComponent (Vector n xs) =
  Vector n (map simplifyComponent $ combineGaps xs)

-- | Combine sequential Gap Components
--
-- >>> combineGaps  [Gap 1, Gap 2, Gap 3, Scalar 2, Gap 1, Gap 2]
-- [Gap 6,Scalar 2,Gap 3]
combineGaps :: [Component] -> [Component]
combineGaps []               = []
combineGaps (Gap n:Gap m:xs) = combineGaps (Gap (n + m) : xs)
combineGaps (x:xs)           = x : combineGaps xs

data TS = TS
  { num :: Integer
  , den :: Integer
  } deriving (Eq, Ord, Show)

isValid :: TS -> Bool
isValid (TS n d) = n > 0 && d > 0

isPowOfTwo :: Integer -> Bool
isPowOfTwo n = n > 0 && n Data.Bits..&. (n - 1) == 0

isTSDenPowOfTwo :: TS -> Bool
isTSDenPowOfTwo (TS _ d) = isPowOfTwo d

toDur :: TS -> Dur
toDur (TS n d) = n % d

data RMeasure = RMeasure
  { ts :: TS
  , rt :: [Component]
  } deriving (Eq, Ord, Show)

-- | TimeSignature examples
-- >>> TS 4 4
-- TS {num = 4, den = 4}
-- >>> TS 3 8
-- TS {num = 3, den = 8}
-- | Check if a TimeSignature is valid
-- >>> isValid (TS 4 4)
-- True
-- >>> isValid (TS (-4) 4)
-- False
-- >>> isValid (TS 4 0)
-- False
-- | Check if the denominator of a TimeSignature is a power of two
-- >>> isTSDenPowOfTwo (TS 4 4)
-- True
-- >>> isTSDenPowOfTwo (TS 4 3)
-- False
-- | Convert a TimeSignature to a Duration
-- >>> toDur (TS 4 4)
-- 1 % 1
-- >>> toDur (TS 3 8)
-- 3 % 8
-- | Create a RMeasure
-- >>> RMeasure (TS 4 4) [Scalar 1, Gap 2]
-- RMeasure {ts = TS {num = 4, den = 4}, Components = [Scalar 1,Gap 2]}
data Shape
  = S
  | V [Shape]
  deriving (Eq, Ord, Show)

shapeOf :: Component -> Shape
shapeOf (Scalar _)    = S
shapeOf (Gap _)       = S
shapeOf (Vector _ xs) = V (map shapeOf xs)

shapeOfArray :: [Component] -> [Shape]
shapeOfArray = map shapeOf

componentsOfArray :: [Component] -> [Int]
componentsOfArray [] = []
componentsOfArray (Scalar n:xs) = n : componentsOfArray xs
componentsOfArray (Gap r:xs) = negate r : componentsOfArray xs
componentsOfArray (Vector n ys:xs) =
  n : componentsOfArray ys ++ componentsOfArray xs

{-
components = [Scalar 5, Gap 3, Vector 4 [Scalar 2, Gap 3, Vector 3 [Gap 2, Scalar 1]]]
shapeOfArray components
--[S,S,V [S,S,V [S,S]]]
-}
{-

-- Simple Scalar and Gap Components
example1 = [Scalar 5, Gap 3]
componentsOfArray example1
-- [5,-3]

-- Nested Vector Components
example2 = [Vector 4 [Scalar 2, Gap 3], Vector 5 [Gap 2, Scalar 1]]
componentsOfArray example2
-- [4,2,-3,5,-2,1]

-- More nested Vector Components
example3 = [Scalar 5, Gap 3, Vector 4 [Scalar 2, Gap 3, Vector 3 [Gap 2, Scalar 1]]]
componentsOfArray example3
-- [5,-3,4,2,-3,3,-2,1]

pPrint example3
-- Empty list
example4 = []
componentsOfArray example4
-- []

--  -}
-- | Function to create a `Component` based on a `Shape` and a list of integers.
fromShapeAndComponents :: Shape -> [Int] -> Maybe Component
-- If the shape is a single Scalar:
fromShapeAndComponents S (x:_)
  | x > 0 = Just $ Scalar x -- Positive value becomes a Scalar.
  | x < 0 = Just $ Gap (-x) -- Negative value becomes a Gap with the absolute value.
  | otherwise = Just $ Gap 0 -- Zero can become a Gap of duration zero, or you could use 'Nothing' here if that makes more sense in context.
-- If the shape is a Vector:
fromShapeAndComponents (V shps) (n:xs) = do
  -- Process each shape in the list of shapes `shps`.
  (components, _) <- foldM go ([], xs) shps
  -- Vector has magnitude of n, with the created components.
  -- Use `abs` to ensure n is positive.
  return $ Vector (abs n) components
  where
    -- Helper function to process each shape.
    go (acc, vals) shape = do
      -- Recursively create a component from the shape.
      val <- fromShapeAndComponents shape vals
      -- Calculate how many integers have been used up by the created component.
      let remaining = drop (length (componentsOfArray [val])) vals
      -- Return the accumulated list of components and the remaining integers.
      return (acc ++ [val], remaining)
-- For other patterns, return Nothing.
fromShapeAndComponents _ _ = Nothing

-- | Function to reconstruct a list of `Component`s from lists of shapes and integers.
reconstruct :: [Shape] -> [Int] -> [Maybe Component]
reconstruct [] _ = [] -- Base case: If there are no shapes, return an empty list.
-- For non-empty list of shapes:
reconstruct (sh:shps) vals =
  case fromShapeAndComponents sh vals of
    -- If creating a component fails, return Nothing and continue with the rest.
    Nothing -> Nothing : reconstruct shps vals
    Just val
      -- Calculate the integers used by the created component.
     ->
      let usedVals = componentsOfArray [val]
          -- Remove the used integers.
          remVals = drop (length usedVals) vals
          -- Return the created component and continue with the remaining shapes and integers.
       in Just val : reconstruct shps remVals

{- -- Test
testArray :: [Component]
testArray =
  [Scalar 5, Gap 3, Vector 4 [Scalar 2, Gap 3, Vector 3 [Gap 2, Scalar 1]]]

shapes :: [Shape]
shapes = map shapeOf testArray

vals' :: [Int]
vals' = componentsOfArray testArray

reconstructedComponentList :: Maybe [Component]
reconstructedComponentList = sequence $ reconstruct shapes vals'
-- Just [Scalar 5,Gap 3,Vector 4 [Scalar 2,Gap 3,Vector 3 [Gap 2,Scalar 1]]]
-}
{-
example1 :: Maybe Component
example1 = fromShapeAndComponents S [5]
-- Expected output: Just (Scalar 5)

example2 :: Maybe Component
example2 = fromShapeAndComponents (V [S, S]) [4, 2, -3]
-- Just (Vector 4 [Scalar 2,Gap 3])
 -}
data RankedLeaf =
  RankedLeaf Int Component -- Int represents the rank or depth.
  deriving (Eq, Ord, Show)

extractRankedLeaves :: Component -> [RankedLeaf]
extractRankedLeaves = go (-1)
  where
    go rank (Scalar n) = [RankedLeaf rank (Scalar n)]
    go rank (Gap n) = [RankedLeaf rank (Gap n)]
    go rank (Vector n components)
      -- Increment the rank for each deeper level and recursively extract leaves.
     = concatMap (go (rank + 1)) components

-- extractRankedLeaves = go 0
-- This function extracts the ranks from a single RankedLeaf
getRank :: RankedLeaf -> Int
getRank (RankedLeaf rank _) = rank

-- This function takes a list of Component and returns a list of Int (ranks)
extractRanks :: [Component] -> [Int]
extractRanks = concatMap (map getRank . extractRankedLeaves)
-- extractRanks components =
--   [ rank | RankedLeaf rank _ <- concatMap extractRankedLeaves components ]
{-
example3 :: Component
example3 = Vector 4 [Scalar 2, Gap (-3), Vector 3 [Scalar 2, Gap (-1)]]

ranks = extractRankedLeaves example3

pPrint ranks

[ RankedLeaf 1
    ( Scalar 2 )
, RankedLeaf 1
    ( Gap
        ( -3 )
    )
, RankedLeaf 2
    ( Scalar 2 )
, RankedLeaf 2
    ( Gap
        ( -1 )
    )
]

 -}

 {-
simpleVector :: Component
simpleVector = Vector 3 [Scalar 2, Gap 4, Scalar 6]

nestedVector :: Component
nestedVector = Vector 7 [Scalar 2, Vector 3 [Scalar 4, Gap 1], Scalar 6]


example2 :: [Int]
example2 = extractRanks [simpleVector]  -- Expected output: [0, 0, 0]

-- Extracting ranks from the nested vector. We should get:
-- - Scalar 2 at top level => 0
-- - Vector 3 itself at top level, but it has nested components
--   - Scalar 4 at depth 1 => 1
--   - Gap 1 at depth 1 => 1
-- - Scalar 6 at top level => 0
example3 :: [Int]
example3 = extractRanks [nestedVector]  -- Expected output: [0, 1, 1, 0]
 -}