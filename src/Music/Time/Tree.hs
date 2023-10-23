module Music.Time.Tree where

import           Data.Tree
import qualified Data.Tree as Tree

-- | ComponentLabel represents individual nodes in our Component tree.
data ComponentLabel
  = Scalar Int
  | Gap Int
  | Vector Int
  deriving (Eq, Show, Ord)

type Component = Tree.Tree ComponentLabel

-- | Extracts the shape of a Component.
shape :: Component -> [Int]
shape (Tree.Node (Scalar _) _)        = [0]
shape (Tree.Node (Gap _) _)           = [0]
shape (Tree.Node (Vector v) children) = v : concatMap shape children

-- | Extracts the values of a Component.
values :: Component -> [Int]
values (Tree.Node (Scalar value) _)      = [value]
values (Tree.Node (Gap value) _)         = [-value]
values (Tree.Node (Vector val) children) = val : concatMap values children

-------------------------------------------------------------------------------
-- Reconstruction Functions
-------------------------------------------------------------------------------
-- | Reconstructs a Component from shape and values.
reconstructComponent :: [Int] -> [Int] -> Component
reconstructComponent shape values =
  let (tree, _, _) = reconstructComponentHelper shape values
   in tree

reconstructComponentHelper :: [Int] -> [Int] -> (Component, [Int], [Int])
reconstructComponentHelper (0:shapeRest) (v:valuesRest)
  | v >= 0 = (Tree.Node (Scalar v) [], shapeRest, valuesRest)
  | otherwise = (Tree.Node (Gap (-v)) [], shapeRest, valuesRest)
reconstructComponentHelper (n:shapeRest) (v:valuesRest)
  | n > 0 =
    let (children, remainingShape, remainingValues) =
          extractComponents n shapeRest valuesRest
     in (Tree.Node (Vector v) children, remainingShape, remainingValues)
  | otherwise =
    error
      $ "Invalid shape or values: Shape="
          ++ show shapeRest
          ++ " Values="
          ++ show valuesRest
reconstructComponentHelper _ _ = error "Mismatch between shape and values."

extractComponents :: Int -> [Int] -> [Int] -> ([Component], [Int], [Int])
extractComponents 0 s v = ([], s, v)
extractComponents n shape values =
  let (tree, newShape, newValues) = reconstructComponentHelper shape values
      (trees, finalShape, finalValues) =
        extractComponents (n - 1) newShape newValues
   in (tree : trees, finalShape, finalValues)

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------
doubleScalars :: Component -> Component
doubleScalars = fmap doubleWhereScalar
  where
    doubleWhereScalar (Scalar n) = Scalar (n * 2)
    doubleWhereScalar label      = label

depthOfTree :: Component -> Int
depthOfTree (Tree.Node _ [])   = 1
depthOfTree (Tree.Node _ subs) = 1 + maximum (map depthOfTree subs)

sumOfScalars :: Component -> Int
sumOfScalars = sum . map scalarValue . Tree.flatten
  where
    scalarValue (Scalar n) = n
    scalarValue _          = 0

flattenTree :: Component -> [ComponentLabel]
flattenTree = Tree.flatten

printTree :: Component -> IO ()
printTree = putStrLn . drawTree . fmap show

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
exampleComponent1 :: Component
exampleComponent1 =
  Tree.Node
    (Vector 3)
    [ Tree.Node (Scalar 1) []
    , Tree.Node (Gap 2) []
    , Tree.Node (Vector 2) [Tree.Node (Scalar 3) [], Tree.Node (Gap 4) []]
    ]

shapeExample1 :: [Int]
shapeExample1 = shape exampleComponent1

-- [3,0,0,2,0,0]
valuesExample1 :: [Int]
valuesExample1 = values exampleComponent1

-- [3,1,-2,2,3,-4]
reconstructedComponent1 :: Component
reconstructedComponent1 = reconstructComponent shapeExample1 valuesExample1
-- Node {rootLabel = Vector 3, subForest = [Node {rootLabel = Scalar 1, subForest = []},Node {rootLabel = Gap 2, subForest = []},Node {rootLabel = Vector 2, subForest = [Node {rootLabel = Scalar 3, subForest = []},Node {rootLabel = Gap 4, subForest = []}]}]}
{-

 flattenTree exampleComponent1
[Vector 3,Scalar 1,Gap 2,Vector 2,Scalar 3,Gap 4]


 printTree (doubleScalars exampleComponent1)



printTree exampleComponent1

Vector 3
|
+- Scalar 1
|
+- Gap 2
|
`- Vector 2
   |
   +- Scalar 3
   |
   `- Gap 4




>>> depthOfTree exampleComponent1

3







sumOfScalars exampleComponent1
4

-}
