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
import Data.List (transpose, intercalate)
import Music.Time.TS 

type Dur = Rational -- ^ Duration

data Component
  = Scalar Int
  | Gap Int
  | Vector Int [Component]
  deriving (Eq, Show, Ord)

-- instance Show Component where
--   show (Scalar n) = " " ++ show n
--   show (Gap n) = " -" ++ show n
--   show (Vector n xs) = " vector " ++ show n  ++ show xs



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



newtype Proportions = Proportions
  { _components :: [Component]
  } deriving (Eq, Ord)
makeLenses ''Proportions

instance Show Proportions where
  show (Proportions xs) = " " ++ show xs

data Capsule = Capsule
  { _ts :: TimeSignature 
  , _proportions :: Proportions
  } deriving (Eq, Ord)
makeLenses ''Capsule

instance Show Capsule where
  show (Capsule tsg props) = show tsg ++ " " ++ show props

data ScoreMeasure = ScoreMeasure
  { _scoreMeasureTs :: TimeSignature
  , _scoreMeasureRhythms :: [Proportions]
  } deriving (Eq, Ord, Show)
makeLenses ''ScoreMeasure

type ScoreVoice = [Capsule]

type ScoreMatrix = [ScoreMeasure]

newtype MatrixScore =
  MatrixScore [[(TimeSignature, Proportions)]]
  deriving (Eq, Ord, Show)

-- newtype Matrix = Matrix [[[Component]]]
--   deriving (Eq, Ord, Show)


-- newtype TensorProportions = TensorProportions [Matrix Proportions]
--   deriving (Eq, Ord, Show)

newtype ProportionsRow = ProportionsRow [Proportions]
  deriving (Eq, Ord, Show)

newtype ProportionsMatrix = ProportionsMatrix [ProportionsRow]
  deriving (Eq, Ord, Show)

newtype ProportionsTensor = ProportionsTensor [ProportionsMatrix]
  deriving (Eq, Ord, Show)


{- 
-- Define a function to access the Proportions at a specific row and column
getProportions :: ProportionsMatrix -> Int -> Int -> Proportions
getProportions (ProportionsMatrix rows) rowIndex colIndex =
  let row = rows !! rowIndex
  in case row of
    ProportionsRow proportionsRow -> proportionsRow !! colIndex

-- :Accessing a Proportions in the matrix
let matrix = ProportionsMatrix [row1, row2]
let rowIndex = 0
let colIndex = 1
let result = getProportions matrix rowIndex colIndex
print result -- This will print the Proportions at row 0, column 1

-- Define a function to access the ProportionsMatrix at a specific index
getProportionsMatrix :: ProportionsTensor -> Int -> ProportionsMatrix
getProportionsMatrix (ProportionsTensor matrices) matrixIndex =
  matrices !! matrixIndex

-- Accessing a ProportionsMatrix in the tensor
let tensor = ProportionsTensor [matrix1, matrix2]
let matrixIndex = 1
let resultMatrix = getProportionsMatrix tensor matrixIndex
print resultMatrix -- This will print the ProportionsMatrix at index 1 of the tensor

-- Define a function to extract a submatrix from a matrix
sliceMatrix :: Matrix a -> Int -> Int -> Int -> Int -> Matrix a
sliceMatrix (Matrix rows) startRow endRow startCol endCol =
  Matrix $ take (endRow - startRow + 1) $ drop startRow $
  map (\row -> take (endCol - startCol + 1) $ drop startCol row) rows

-- Slicing a submatrix from a matrix
let matrix = Matrix [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let slicedMatrix = sliceMatrix matrix 0 1 1 2
print slicedMatrix

-- Define a function to extract a subtensor from a tensor
sliceTensor :: ProportionsTensor -> Int -> Int -> Int -> Int -> Int -> Int -> ProportionsTensor
sliceTensor (ProportionsTensor matrices) startMatrix endMatrix startRow endRow startCol endCol =
  ProportionsTensor $ take (endMatrix - startMatrix + 1) $ drop startMatrix $
  map (\matrix -> sliceMatrix matrix startRow endRow startCol endCol) matrices

-- Slicing a subtensor from a tensor
let tensor = ProportionsTensor [matrix1, matrix2]
let slicedTensor = sliceTensor tensor 0 0 0 1 1 2
print slicedTensor

 -}
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


-- Define some sample components
comp1 :: Component
comp1 = Scalar 2

comp2 :: Component
comp2 = Gap 3

comp3 :: Component
comp3 = Vector 4 [Scalar 2, Gap 3]

comp4 :: Component
comp4 = Vector 5 [Gap 2, Scalar 1]

-- Create a Matrix of components
matrix1c :: Matrix Component
matrix1c = Matrix [
    [ [comp1, comp2]
    , [comp3, comp4]
  ], 
  [ [comp1, comp2]
  , [comp3, comp4]
  ]]


data Tree a = Node a [Tree a]
  deriving (Show)

-- Convert a Component to a tree node
componentToTreeNode :: Component -> Tree String
componentToTreeNode comp = case comp of
  Scalar n -> Node ("Scalar " ++ show n) []
  Gap n -> Node ("Gap " ++ show n) []
  Vector n components -> Node ("Vector " ++ show n) (map componentToTreeNode components)

-- Convert a Matrix Component to a tree
matrixToTree :: [[Component]] -> Tree String
matrixToTree rows = Node "Matrix" (map (Node "Row" . map componentToTreeNode) rows)

-- Pretty-print a tree
prettyPrintTree :: Tree String -> String
prettyPrintTree (Node label children) =
  label ++ if null children then "\n" else ":\n" ++ indent (concatMap prettyPrintTree children)
  where
    indent = unlines . map ("  "++) . lines


-- Example usage:
matrix1d :: [[Component]]
matrix1d =
  [ [ Scalar 2, Gap 3 ]
  , [ Vector 4 [Scalar 2, Gap 3], Vector 5 [Gap 2, Scalar 1] ]
  ]

-- pPrint matrix1c 


test :: IO ()
test = putStrLn $ prettyPrintTree (matrixToTree matrix1d)


-- Matrix:
--   Row:
--     Scalar 2
--     Gap 3
--   Row:
--     Vector 4:
--       Scalar 2
--       Gap 3
--     Vector 5:
--       Gap 2
--       Scalar 1

-- Testing Lenses 



addScalar :: Proportions -> Component -> Proportions
addScalar props scalar = props & components %~ (scalar:)

updateRhythm :: Capsule -> Proportions -> Capsule
updateRhythm capsule newRhythm = capsule & proportions .~ newRhythm

getTimeSignature :: ScoreMeasure -> TimeSignature
getTimeSignature scoreMeasure = scoreMeasure ^. scoreMeasureTs

changeRhythms :: ScoreMeasure -> [Proportions] -> ScoreMeasure
changeRhythms scoreMeasure newRhythms = scoreMeasure & scoreMeasureRhythms .~ newRhythms

firstComponent :: Capsule -> Maybe Component
firstComponent capsule = capsule ^? proportions . components . ix 0


getCapsule :: MatrixScore -> Int -> Int -> Maybe (TimeSignature, Proportions)
getCapsule (MatrixScore m) rowIndex colIndex = m ^? ix rowIndex . ix colIndex


-- getCapsule matrix1 1 1
-- Just (TS 3//4,Proportions {_components = [Vector 3 [Scalar 1,Gap 2,Vector 2 [Scalar 1,Gap 1]],Scalar 3]})


-- Access the Proportions inside a Matrix
getProportions :: MatrixScore -> Int -> Int -> Maybe Proportions
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



ts1 :: TimeSignature
ts1 = TimeSignature 4 4

ts2 :: TimeSignature
ts2 = 6 // 8

ts3 :: TimeSignature
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
