{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Music.Time.RT where

import           Control.Monad      (foldM)
import           Data.Bits          ((.&.))
import           Data.Ratio
import           Test.QuickCheck
import           Text.Pretty.Simple
import Control.Lens
import Data.List (transpose)

type Dur = Rational -- ^ Duration

data Component
  = Scalar Int
  | Gap Int
  | Vector Int [Component]
  deriving (Eq, Ord)

instance Show Component where
  show (Scalar n) = " " ++ show n
  show (Gap n) = " -" ++ show n
  show (Vector n xs) = " vector " ++ show n  ++ show xs



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


-- | TimeSignature
data TS = TS
  { _num :: Integer
  , _den :: Integer
  } deriving (Eq, Ord)
makeLenses ''TS

-- instance Show TS where
--   show (TS n d) = " " ++ show n ++ "//" ++ show d

instance Show TS where
  show (TS n d) = " time " ++ show n ++ "/" ++ show d

infixr 7 //

-- | Time Signature 
(//) :: Integer -> Integer -> TS
n // d = TS n d


{- 
>>> ts1 = 4 // 4
>>> ts2 = ts1 & num .~ 7 & den .~ 8
>>> ts2
TS 7//8

 -}
isValidTS :: TS -> Bool
isValidTS (TS n d) = n > 0 && d > 0

isPowOfTwo :: Integer -> Bool
isPowOfTwo n = n > 0 && n Data.Bits..&. (n - 1) == 0

isTSDenPowOfTwo :: TS -> Bool
isTSDenPowOfTwo (TS _ d) = isPowOfTwo d

timeSigToDur :: TS -> Dur
timeSigToDur (TS n d) = n % d

checkPowerOfTwo :: TS -> Bool
checkPowerOfTwo ts = isPowOfTwo $ ts ^. den


newtype Proportions = Proportions
  { _components :: [Component]
  } deriving (Eq, Ord)
makeLenses ''Proportions

instance Show Proportions where
  show (Proportions xs) = " " ++ show xs

data Capsule = Capsule
  { _ts :: TS
  , _proportions :: Proportions
  } deriving (Eq, Ord)
makeLenses ''Capsule

instance Show Capsule where
  show (Capsule tsg props) = show tsg ++ " " ++ show props

data ScoreMeasure = ScoreMeasure
  { _scoreMeasureTs :: TS
  , _scoreMeasureRhythms :: [Proportions]
  } deriving (Eq, Ord, Show)
makeLenses ''ScoreMeasure

type ScoreVoice = [Capsule]

type ScoreMatrix = [ScoreMeasure]

newtype MatrixScore =
  MatrixScore [[(TS, Proportions)]]
  deriving (Eq, Ord, Show)

-- newtype Matrix = Matrix [[[Component]]]
--   deriving (Eq, Ord, Show)

newtype Matrix a = Matrix [[[a]]]
  deriving (Eq, Ord, Show)

matrix :: [[[a]]] -> Matrix a
matrix = Matrix

-- unwraps the 2d list from a matrix
unMatrix :: Matrix a -> [[[a]]]
unMatrix (Matrix m) = m



-- lifts a 2D list operation to be a Matrix operation
liftMatrixOp :: ([[a]] -> [[a]]) -> Matrix a -> Matrix a
liftMatrixOp f (Matrix xs) = Matrix $ map f xs


-- Testing Lenses 


addScalar :: Proportions -> Component -> Proportions
addScalar props scalar = props & components %~ (scalar:)

updateRhythm :: Capsule -> Proportions -> Capsule
updateRhythm capsule newRhythm = capsule & proportions .~ newRhythm

getTimeSignature :: ScoreMeasure -> TS
getTimeSignature scoreMeasure = scoreMeasure ^. scoreMeasureTs

changeRhythms :: ScoreMeasure -> [Proportions] -> ScoreMeasure
changeRhythms scoreMeasure newRhythms = scoreMeasure & scoreMeasureRhythms .~ newRhythms

firstComponent :: Capsule -> Maybe Component
firstComponent capsule = capsule ^? proportions . components . ix 0


getCapsule :: MatrixScore -> Int -> Int -> Maybe (TS, Proportions)
getCapsule (MatrixScore m) rowIndex colIndex = m ^? ix rowIndex . ix colIndex


-- getCapsule matrix1 1 1
-- Just (TS 3//4,Proportions {_components = [Vector 3 [Scalar 1,Gap 2,Vector 2 [Scalar 1,Gap 1]],Scalar 3]})


-- Access the Proportions inside a Matrix
getProportions :: MatrixScore-> Int -> Int -> Maybe Proportions
getProportions (MatrixScore m) rowIndex colIndex = m ^? ix rowIndex . ix colIndex . _2

-- getProportions matrix1 1 1

-- VerticalSlice ?
-- "
--     | Measure 1 | Measure 2 | Measure 3 | ...
-- ----------------------------------------------
-- Voice 1 |   M1,1   |   M1,2   |   M1,3   | ...
-- ----------------------------------------------
-- Voice 2 |   M2,1   |   M2,2   |   M2,3   | ...
-- ----------------------------------------------
-- Voice 3 |   M3,1   |   M3,2   |   M3,3   | ...
-- ----------------------------------------------
--    .       .          .          .
--    .       .          .          .
-- "



-- Example 1: Simple 2x2 matrix
matrix1b :: Matrix Component
matrix1b =
  Matrix
    [ [ [Scalar 1, Gap 2]
      , [Vector 3 [Scalar 4, Gap 5], Scalar 6]
      ]
    , [ [Scalar 7, Scalar 8]
      , [Scalar 9, Scalar 10]
      ]
    ]

-- Example 2: 3x3 matrix with nested vectors
matrix2 :: Matrix Component
matrix2 =
  Matrix
    [ [ [Scalar 1, Scalar 2, Scalar 3]
      , [Scalar 4, Scalar 5, Scalar 6]
      , [Scalar 7, Scalar 8, Scalar 9]
      ]
    , [ [Vector 2 [Scalar 10, Scalar 11], Scalar 12]
      , [Scalar 13, Scalar 14, Vector 3 [Scalar 15, Scalar 16, Scalar 17]]
      , [Vector 4 [Scalar 18, Vector 2 [Scalar 19, Scalar 20]], Scalar 21]
      ]
    ]

-- Example 3: Irregular matrix with varying row lengths
matrix3 :: Matrix Component
matrix3 =
  Matrix
    [ [ [Scalar 1, Scalar 2]
      , [Scalar 3]
      , [Vector 2 [Scalar 4, Scalar 5]]
      ]
    , [ [Scalar 6, Scalar 7, Scalar 8, Scalar 9]
      , [Vector 3 [Scalar 10, Scalar 11, Scalar 12]]
      ]
    ]

-- Example 5: A single row matrix
matrix5 :: Matrix Component
matrix5 =
  Matrix
    [ [ [Scalar 1, Scalar 2, Scalar 3]
      ]
    ]

-- Example 6: A single column matrix
matrix6 :: Matrix Component
matrix6 =
  Matrix
    [ [ [Scalar 1]
      ]
    , [ [Scalar 2]
      ]
    , [ [Scalar 3]
      ]
    ]

{- -- Example 7: Transpose of matrix1
matrix1Transpose :: MatrixP
matrix1Transpose = MatrixP (transpose (unMatrixP matrix1b))

-- Example 8: Transpose of matrix2
matrix2Transpose :: MatrixP
matrix2Transpose = MatrixP (transpose (unMatrixP matrix2))

-- Example 9: Transpose of matrix3
matrix3Transpose :: MatrixP
matrix3Transpose = MatrixP (transpose (unMatrixP matrix3))
 -}



ts1 :: TS
ts1 = TS 4 4

ts2 :: TS
ts2 = 6 // 8

ts3 :: TS
ts3 = 3 // 4

prop1 :: Proportions
prop1 = Proportions [Scalar 2, Gap 1, Scalar 1]

prop2 :: Proportions
prop2 = Proportions [Scalar 1, Scalar 2, Scalar 1]

prop3 :: Proportions
prop3 = Proportions [Vector 4 [Scalar 2, Gap 3], Vector 5 [Gap 2, Scalar 1]]

prop4 :: Proportions
prop4 = Proportions [Vector 3 [Scalar 1, Gap 2, Vector 2 [Scalar 1, Gap 1]], Scalar 3]


capsule1 :: Capsule
capsule1 = Capsule ts1 prop1

capsule2 :: Capsule
capsule2 = Capsule ts2 prop2

capsule3 :: Capsule
capsule3 = Capsule ts3 prop3

capsule4 :: Capsule
capsule4 = Capsule ts1 prop4


scoreMeasure1 :: ScoreMeasure
scoreMeasure1 = ScoreMeasure ts1 [prop1, prop2, prop3]

scoreMeasure2 :: ScoreMeasure
scoreMeasure2 = ScoreMeasure ts2 [prop2, prop4, prop3]

scoreMeasure3 :: ScoreMeasure
scoreMeasure3 = ScoreMeasure ts3 [prop1, prop3, prop4]


scoreVoice1 :: ScoreVoice
scoreVoice1 = [capsule1, capsule3, capsule4]

scoreVoice2 :: ScoreVoice
scoreVoice2 = [capsule2, capsule3]


scoreMatrix1 :: ScoreMatrix
scoreMatrix1 = [scoreMeasure1, scoreMeasure2, scoreMeasure3]


matrix1 :: MatrixScore
matrix1 = MatrixScore [[(ts1, prop1), (ts2, prop2)], [(ts2, prop3), (ts3, prop4)]]


-- pPrint scoreMatrix1

-- pPrint matrix1



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
