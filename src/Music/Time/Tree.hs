{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Music.Time.Tree where

import           Data.Foldable (find)
import           Data.List     (nub)
import           Data.Ratio
import           Data.Tree
import qualified Data.Tree     as Tree

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

sumAtRank :: Int -> Component -> Int
sumAtRank rank tree = sumAtRankHelper rank tree 0

sumAtRankHelper :: Int -> Component -> Int -> Int
sumAtRankHelper rank (Tree.Node label subs) acc =
  let newAcc =
        if rank == 0
          then acc + scalarValue label
          else acc
   in if rank == 0
        then newAcc
        else sum (map (\sub -> sumAtRankHelper (rank - 1) sub newAcc) subs)
               + newAcc
  where
    scalarValue (Scalar n) = n
    scalarValue _          = 0

{-
tree :: Component
tree = Tree.Node (Scalar 5) [
    Tree.Node (Scalar 3) [
      Tree.Node (Scalar 1) [],
      Tree.Node (Scalar 2) []
    ],
    Tree.Node (Scalar 2) []
 ]

sumAtRank 1 tree

 -}
-- |
-- >>> partitionRational (1 % 1) [1, 2, 1]
-- [1 % 4,1 % 2,1 % 4]
partitionRational :: Rational -> [Int] -> [Rational]
partitionRational inteiro lista = map (* inteiro) normalizedList
  where
    partitionSum = sum lista
    ratio = inteiro / toRational partitionSum :: Rational
    normalizedList = map ((* ratio) . toRational) lista

flattenTree :: Component -> [ComponentLabel]
flattenTree = Tree.flatten

printTree :: Component -> IO ()
printTree = putStrLn . drawTree . fmap show

filterOutGaps :: Component -> Component
filterOutGaps (Tree.Node label subs) =
  let filteredChildren = map filterOutGaps (filter isNotGap subs)
   in Tree.Node label filteredChildren
  where
    isNotGap (Tree.Node (Gap _) _) = False
    isNotGap _                     = True

maxScalar :: Component -> Int
maxScalar = maximum . map scalarValue . Tree.flatten
  where
    scalarValue (Scalar n) = n
    scalarValue _          = minBound :: Int

replaceScalar :: Int -> Int -> Component -> Component
replaceScalar target replacement = fmap replaceWhereScalar
  where
    replaceWhereScalar (Scalar n)
      | n == target = Scalar replacement
      | otherwise = Scalar n
    replaceWhereScalar label = label

--  printTree $ replaceScalar 1 10 exampleComponent1
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
  case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs

-- import Data.Maybe (mapMaybe)
filterComponents :: (Component -> Bool) -> Component -> Component
filterComponents predicate = go
  where
    go node@(Tree.Node label children)
      | predicate node = Tree.Node label (map go children)
      | otherwise = Tree.Node (Vector 0) [] -- Replace non-matching nodes with an empty vector

{-


ghci> let hasValue2 (Tree.Node (Scalar n) _) = n == 2
ghci> let filtered = filterComponents hasValue2 exampleComponent1
ghci> printTree filtered
Vector 3
|
+- Scalar 2
|
+- Gap 2
|
`- Vector 2
   |
   +- Gap 4
   |
   `- Gap 3



 -}
-- findNodeByValue' :: Int -> Component -> Maybe Component
-- findNodeByValue' val = find (\node -> case node of
--                                       Tree.Node (Scalar n) _ | n == val -> True
--                                       Tree.Node (Gap g) _    | g == (-val) -> True
--                                       _ -> False)
multiplyScalars :: Int -> Component -> Component
multiplyScalars factor = fmap multiplyWhereScalar
  where
    multiplyWhereScalar (Scalar n) = Scalar (n * factor)
    multiplyWhereScalar label      = label

{-

ghci> let multiplied = multiplyScalars 2 exampleComponent1
ghci> printTree multiplied
Vector 3
|
+- Scalar 2
|
+- Gap 2
|
`- Vector 2
   |
   +- Scalar 6
   |
   `- Gap 4


  -}
findNodeByValue :: Int -> Component -> Maybe Component
findNodeByValue val (node@(Tree.Node (Scalar n) _))
  | n == val = Just node
findNodeByValue val (node@(Tree.Node (Gap g) _))
  | g == (-val) = Just node
findNodeByValue val (Tree.Node _ subs) =
  case mapMaybe (findNodeByValue val) subs of
    (found:_) -> Just found
    []        -> Nothing

-- printTree $ replaceScalar 1 10 exampleComponent1
reverseScalars :: Component -> Component
reverseScalars = fmap reverseWhereScalar
  where
    reverseWhereScalar (Scalar n) = Scalar n
    reverseWhereScalar label      = label

-- printTree (reverseScalars exampleComponent1)
--  findNodeByValue 3 exampleComponent1
countVectors :: Component -> Int
countVectors (Tree.Node (Vector _) subs) = 1 + sum (map countVectors subs)
countVectors (Tree.Node _ subs)          = sum (map countVectors subs)

--  countVectors exampleComponent1
transformGaps :: (Int -> Int) -> Component -> Component
transformGaps f = fmap transformWhereGap
  where
    transformWhereGap (Gap g) = Gap (f g)
    transformWhereGap label   = label

--  printTree $ transformGaps (`div` 2) exampleComponent1
{-
Vector 3
|
+- Scalar 1
|
+- Gap 1
|
`- Vector 2
   |
   +- Scalar 3
   |
   `- Gap 2 -}
countLeaves :: Component -> Int
countLeaves (Tree.Node _ [])   = 1
countLeaves (Tree.Node _ subs) = sum (map countLeaves subs)

-- countLeaves exampleComponent1
allScalars :: Component -> [Int]
allScalars = mapMaybe scalarValue . Tree.flatten
  where
    scalarValue (Scalar n) = Just n
    scalarValue _          = Nothing

-- allScalars exampleComponent1
-- Mapping Components to a New Type:
mapToNewType :: (ComponentLabel -> newLabel) -> Component -> Tree newLabel
mapToNewType f = fmap f

{-

ghci> let mapScalarToString (Scalar n) = "Value: " ++ show n
ghci> let mappedTree = mapToNewType mapScalarToString exampleComponent1
ghci> printTree mappedTree
Vector 3
|
+- Value: 1
|
+- Gap 2
|
`- Vector 2
   |
   +- Value: 3
   |
   `- Gap 4

 -}
replaceSubtree :: Component -> Component -> Component -> Component
replaceSubtree target replacement tree = go tree
  where
    go (Node label children)
      | tree == target = replacement
      | otherwise = Node label (map go children)

{-

ghci> let replacementTree = Vector 2 [Scalar 10, Gap 20]
ghci> let updatedTree = replaceSubtree (Vector 2 [Scalar 3, Gap 4]) replacementTree exampleComponent1
ghci> printTree updatedTree
Vector 3
|
+- Scalar 1
|
`- Vector 2
   |
   +- Scalar 10
   |
   `- Gap 20


 -}
breadthFirstTraversal :: Component -> [Component]
breadthFirstTraversal root = bfs [root]
  where
    bfs [] = []
    bfs xs = xs ++ bfs (nub $ concatMap Tree.subForest xs)

{-
ghci> let breadthFirst = breadthFirstTraversal exampleComponent1
ghci> mapM_ printTree breadthFirst
Vector 3
Scalar 1
Gap 2
Vector 2
Scalar 3
Gap 4


 -}
replaceScalar2 :: Int -> Int -> Component -> Component
replaceScalar2 old new (Tree.Node (Scalar n) subs)
  | n == old = Tree.Node (Scalar new) subs
replaceScalar2 old new (Tree.Node label subs) =
  Tree.Node label (map (replaceScalar2 old new) subs)

-- -- printTree $ replaceScalar2 3 9 exampleComponent1
-- levels2 :: Component -> [[ComponentLabel]]
-- levels2 = fmap (fmap rootLabel) . Music.Time.Tree.levels
levels :: Component -> [[ComponentLabel]]
levels tree = go [tree]
  where
    go []    = []
    go nodes = map Tree.rootLabel nodes : go (concatMap Tree.subForest nodes)

-- Music.Time.Tree.levels exampleComponent1
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


Node {rootLabel = Vector 3, subForest = [Node {rootLabel = Scalar 1, subForest = []},Node {rootLabel = Gap 2, subForest = []},Node {rootLabel = Vector 2, subForest = [Node {rootLabel = Scalar 3, subForest = []},Node {rootLabel = Gap 4, subForest = []}]}]}






sumOfScalars exampleComponent1
4

-}
