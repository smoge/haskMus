module Rtm.Common
  ( RtmLabel (..),
    Rtm,
    showRtm,
    scalar,
    gap,
    vector,
    s,
    g,
    v,
    rtm',
    printRtm,
    rtmDepth,
    (|:),
    isValidRtm,
    countRtmScalars,
    noRtmGaps,
    collapseRtmGaps,
    extractIntsFromLabels,
    reconstructLabelsFromInts,
    setRtmLabelInt
  )
where

import Data.Data
import Data.Maybe (mapMaybe)
import Data.Tree

{- ----------------------------------- tests -----------------------------------------------------
showRtm $ rtm' [s 1, g 3, 2 |: [s 1, g 1, s 1], s 1]

-- "(1 -3 (2 (1 -1 1)) 1)"

rtm' [s 1, g 3, 2 |: [s 1, g 1, s 1], s 1]

isValidRtm $ rtm' [s 1, g 3, 2 |: [s 1, g 1, s 1], s 1]

------------------------------------------------------------------------------------------------} 


data RtmLabel
  = RtmScalar Int
  | RtmGap Int
  | RtmVector Int
  | RtmCons
  deriving (Eq, Show, Data)

type Rtm = Tree RtmLabel


showRtm :: Rtm -> String
showRtm (Node (RtmScalar num) []) = show num
showRtm (Node (RtmGap num) []) = "-" <> show num
showRtm (Node (RtmVector num) children) = "(" <> show num <> " " <> "(" <> unwords (fmap showRtm children) <> "))"
showRtm (Node RtmCons children) = "(" <> unwords (fmap showRtm children) <> ")"
showRtm _ = error "Invalid Rtm structure"

-- | Scalar constructor.
scalar, s :: Int -> Rtm
scalar x = if x == 0 then Node (RtmGap 0) [] else Node (RtmScalar (abs x)) []
s = scalar

-- | Gap constructor.
gap, g :: Int -> Rtm
gap x = Node (RtmGap (abs x)) []
g = gap

-- | RtmVector constructor.
vector, v :: Int -> [Rtm] -> Rtm
vector x cs = if x == 0 then Node (RtmGap 0) [] else Node (RtmVector (abs x)) cs
v = vector

-- | RtmCons constructor.
--
rtm' :: [Rtm] -> Rtm
rtm' = Node RtmCons

-- | Infix operator for constructing an RtmVector.
infixr 5 |:

(|:) :: Int -> [Rtm] -> Rtm
num |: list = vector num list

-- | Check if an Rtm represents a valid RtmScalar.
isValidScalar :: Rtm -> Bool
isValidScalar (Node (RtmScalar num) []) = num >= 1 && num <= 20
isValidScalar _ = False

-- | Check if an Rtm represents a valid RtmGap.
isValidGap :: Rtm -> Bool
isValidGap (Node (RtmGap num) []) = num >= 1 && num <= 20
isValidGap _ = False

-- | Check if an Rtm represents a valid RtmVector.
isValidVector :: Rtm -> Bool
isValidVector (Node (RtmVector num) xss) = num >= 1 && num <= 20 && length xss >= 2
isValidVector _ = False

-- | Check if an Rtm is valid.
isValidRtm :: Rtm -> Bool
isValidRtm = isValidRtm' True
  where
    isValidRtm' _ (Node (RtmScalar num) []) = isValidScalar (Node (RtmScalar num) [])
    isValidRtm' _ (Node (RtmGap num) []) = isValidGap (Node (RtmGap num) [])
    isValidRtm' _ (Node (RtmVector num) cs) = isValidVector (Node (RtmVector num) cs) && all (isValidRtm' False) cs
    isValidRtm' True (Node RtmCons cs) = all (isValidRtm' False) cs -- Only valid if isRoot is True
    isValidRtm' False (Node RtmCons _) = False -- Cons not allowed in descendants
    isValidRtm' _ _ = False

-- | Print an RTM to standard output.
printRtm :: Rtm -> IO ()
printRtm = putStrLn . drawTree . fmap show

-- | Get the depth of an Rtm.
rtmDepth :: Rtm -> Int
rtmDepth (Node _ []) = 1
rtmDepth (Node _ subs) = 1 + maximum (fmap rtmDepth subs)

-- | Given an Rtm, collapses all gaps in the tree by summing consecutive gaps
-- and returning a new Rtm with the gaps collapsed.
collapseRtmGaps :: Rtm -> Rtm
collapseRtmGaps (Node z children) = Node z (processChildren children)
  where
    processChildren [] = []
    processChildren (Node (RtmGap y) [] : xs) =
      let (rests, remainder) = span isRest xs
          total_ = y + sum (fmap getRestValue rests)
       in Node (RtmGap total_) [] : processChildren remainder
    processChildren (x : xs) = collapseRtmGaps x : processChildren xs

    isRest (Node (RtmGap _) []) = True
    isRest _ = False

    getRestValue (Node (RtmGap x) []) = x
    getRestValue _ = 0 -- shouldn't happen, but to make it exhaustive

-- | Remove gaps from an Rtm.
noRtmGaps :: Rtm -> Rtm
noRtmGaps = fmap noGap
  where
    noGap (RtmGap num) = RtmScalar num
    noGap x = x

-- | Count the number of scalar nodes in an Rtm.
countRtmScalars :: Rtm -> Int
countRtmScalars (Node (RtmScalar _) children) = 1 + sum (fmap countRtmScalars children)
countRtmScalars (Node _ children) = sum (fmap countRtmScalars children)

-- | Given a list of RtmLabels, returns a list of Ints extracted from the labels
--   that carry an Int. RtmScalar, RtmGap and RtmVector carry an Int, while
--   RtmCons and any other label do not.
extractIntsFromLabels :: [RtmLabel] -> [Int]
extractIntsFromLabels = mapMaybe extractInt
  where
    extractInt :: RtmLabel -> Maybe Int
    extractInt (RtmScalar n) = Just n
    extractInt (RtmGap n) = Just n
    extractInt (RtmVector n) = Just n
    extractInt _ = Nothing -- RtmCons, or any other non-int-carrying label

-- | Set the integer value of an RtmLabel.
setRtmLabelInt :: Int -> RtmLabel -> RtmLabel
setRtmLabelInt n (RtmScalar _) = RtmScalar n
setRtmLabelInt n (RtmGap _) = RtmGap n
setRtmLabelInt n (RtmVector _) = RtmVector n
setRtmLabelInt _ x = x -- RtmCons

-- | Reconstruct a list of RtmLabels from a list of Ints while retaining the position of RtmCons labels.
reconstructLabelsFromInts :: [Int] -> [RtmLabel] -> [RtmLabel]
reconstructLabelsFromInts ints = snd . foldl insertInt (ints, [])
  where
    insertInt :: ([Int], [RtmLabel]) -> RtmLabel -> ([Int], [RtmLabel])
    insertInt (n : ns, ls) (RtmScalar _) = (ns, ls <> [RtmScalar n])
    insertInt (n : ns, ls) (RtmGap _) = (ns, ls <> [RtmGap n])
    insertInt (n : ns, ls) (RtmVector _) = (ns, ls <> [RtmVector n])
    insertInt (ns, ls) rtmConsLabel = (ns, ls <> [rtmConsLabel]) -- RtmCons or any other label type

-- | An example Rtm.
example :: Rtm
example = rtm' [s 1, g 3, 2 |: [s 1, g 1, s 1], s 1]

main :: IO ()
main = printRtm example

{-
ghci> main
RtmCons
|
+- RtmScalar 1
|
+- RtmGap 3
|
+- RtmVector 2
|  |
|  +- RtmScalar 1
|  |
|  +- RtmGap 1
|  |
|  `- RtmScalar 1
|
`- RtmScalar 1

-}