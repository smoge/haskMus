{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid partial function" #-}

module Selector
  ( Selector,
    runSelector,
    runSelectorWithGen,
    element,
    boolean,
    weighted,
    choose,
    wchoose,
    wchooseWithDefault,
    markov,
    runMarkovChain,
    runMarkovChain',
    normalizeWeights,
  )
where

import Control.Monad (replicateM)
import Data.Bifunctor (second)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.List.NonEmpty qualified as NE
import System.Random (randomR, randomRIO)
import System.Random.TF (TFGen, mkTFGen)

data Selector :: Type -> Type where
  Pure :: a -> Selector a
  Element :: NonEmpty a -> Selector a
  Boolean :: Selector Bool
  Weighted :: NonEmpty (Double, a) -> Selector a
  Sequence :: Selector a -> (a -> Selector b) -> Selector b

instance Functor Selector where
  fmap :: (a -> b) -> Selector a -> Selector b
  fmap f (Pure x) = Pure (f x)
  fmap f (Element xs) = Element (fmap f xs)
  fmap f Boolean = Sequence Boolean (Pure . f)
  fmap f (Weighted xs) = Weighted (fmap (second f) xs)
  fmap f (Sequence m k) = Sequence m (fmap f . k)

instance Applicative Selector where
  pure :: a -> Selector a
  pure = Pure
  (<*>) :: Selector (a -> b) -> Selector a -> Selector b
  (<*>) = ap

instance Monad Selector where
  (>>=) :: Selector a -> (a -> Selector b) -> Selector b
  (>>=) = Sequence

runSelectorWithGen :: TFGen -> Selector a -> a
runSelectorWithGen gen s = case s of
  Pure x -> x
  Element xs ->
    let (index, _) = randomR (0, length xs - 1) gen
     in NE.toList xs !! index
  Boolean ->
    let (b, _) = randomR (False, True) gen
     in b
  Weighted xs ->
    let totalWeight = sum (fmap fst xs)
        (r, _) = randomR (0 :: Double, totalWeight) gen
        go acc ((w, x) : rest)
          | r <= acc + w = x
          | otherwise = go (acc + w) rest
        go _ [] = error "Impossible: weighted selection failed"
     in go 0 (NE.toList xs)
  Sequence m k -> runSelectorWithGen gen2 (k (runSelectorWithGen gen1 m))
    where
      (gen1, gen2) = split gen

runSelector :: Selector a -> IO a
runSelector s = case s of
  Pure x -> pure x
  Element xs -> do
    index <- randomRIO (0, length xs - 1)
    pure $ NE.toList xs !! index
  Boolean -> randomRIO (False, True)
  Weighted xs -> do
    let totalWeight = sum (fmap fst xs)
    r <- randomRIO (0, totalWeight)
    pure $ go 0 (NE.toList xs) r
    where
      go _ [] _ = error "Impossible: weighted selection failed"
      go acc ((w, x) : rest) r
        | r <= acc + w = x
        | otherwise = go (acc + w) rest r
  Sequence m k -> runSelector m >>= \a -> runSelector (k a)

split :: TFGen -> (TFGen, TFGen)
split gen =
  let (i, gen') = randomR (minBound :: Int, maxBound :: Int) gen
   in (mkTFGen i, gen')

element :: NonEmpty a -> Selector a
element = Element

boolean :: Selector Bool
boolean = Boolean

weighted :: NonEmpty (Double, a) -> Selector a
weighted = Weighted

choose :: [a] -> Maybe (Selector a)
choose [] = Nothing
choose (x : xs) = Just $ Element (x :| xs)

wchoose :: [(Double, a)] -> Maybe (Selector a)
wchoose [] = Nothing
wchoose (x : xs) = Just $ Weighted (x :| xs)

wchooseWithDefault :: a -> [(Double, a)] -> IO a
wchooseWithDefault defaultValue weights =
  case wchoose weights of
    Just selector -> runSelector selector
    Nothing -> pure defaultValue

ap :: Selector (a -> b) -> Selector a -> Selector b
ap sf sx = do
  f <- sf
  f <$> sx

markov :: String -> Selector String
markov "A" = weighted (fromList [(0.5, "A"), (0.3, "B"), (0.2, "C")])
markov "B" = weighted (fromList [(0.4, "A"), (0.4, "B"), (0.2, "C")])
markov "C" = weighted (fromList [(0.1, "A"), (0.4, "B"), (0.5, "C")])
markov _ = pure "A"

runMarkovChain :: String -> Int -> Selector [String]
runMarkovChain start steps = replicateM steps (markov start >>= markov)

runMarkovChain' :: (Eq a) => a -> Int -> (a -> [(Double, a)]) -> Selector [a]
runMarkovChain' start steps transitions = replicateM steps (markov_ start >>= markov_)
  where
    markov_ state = case wchoose (transitions state) of
      Just selector -> selector
      Nothing -> pure state

normalizeWeights :: [(Double, a)] -> [(Double, a)]
normalizeWeights weights =
  let total = sum (fmap fst weights)
   in fmap (\(w, x) -> (w / total, x)) weights

main :: IO ()
main = do
  result1 <- runSelector $ element (fromList [1, 2, 3, 4])
  putStrLn $ "Selected element: " <> show result1

  result2 <- runSelector $ weighted (fromList [(0.3, "Red"), (0.5, "Blue"), (0.2, "Green")])
  putStrLn $ "Selected weighted item: " <> result2

  result3 <- runSelector $ runMarkovChain "A" 100
  putStrLn $ "Markov chain result: " <> show result3

  let transitions :: Int -> [(Double, Int)]
      transitions 1 = [(0.5, 1), (0.3, 2), (0.2, 3)]
      transitions 2 = [(0.4, 1), (0.4, 2), (0.2, 3)]
      transitions 3 = [(0.1, 1), (0.4, 2), (0.5, 3)]

  result4 <- runSelector $ runMarkovChain' 1 1000 transitions
  putStrLn $ "Markov chain result: " <> show result4
