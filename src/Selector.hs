{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Selector
  ( Selector
  , runSelector
  , element
  , boolean
  , weighted
  , choose
  , wchoose
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import System.Random (randomRIO)
import Data.Bifunctor (second)

data Selector :: Type -> Type where
  Element  :: NonEmpty a -> Selector a
  Boolean  :: Selector Bool
  Weighted :: NonEmpty (Double, a) -> Selector a

runSelector :: Selector a -> IO a
runSelector s = case s of
  Element xs  -> do
    index <- randomRIO (0, length xs - 1)
    pure $ NE.toList xs !! index
  Boolean     -> randomRIO (False, True)
  Weighted xs -> do
    let totalWeight = sum (fmap fst xs)
    r <- randomRIO (0, totalWeight)
    pure $ go 0 (NE.toList xs) r
    where
      go _ [] _ = error "Impossible: weighted selection failed"
      go acc ((w, x):rest) r
        | r <= acc + w = x
        | otherwise    = go (acc + w) rest r

element :: NonEmpty a -> Selector a
element = Element

boolean :: Selector Bool
boolean = Boolean

weighted :: NonEmpty (Double, a) -> Selector a
weighted = Weighted

choose :: [a] -> Maybe (Selector a)
choose []     = Nothing
choose (x:xs) = Just $ Element (x :| xs)

wchoose :: [(Double, a)] -> Maybe (Selector a)
wchoose []     = Nothing
wchoose (x:xs) = Just $ Weighted (x :| xs)

instance Functor Selector where
  fmap f (Element xs)  = Element (fmap f xs)
  fmap f Boolean       = Weighted ((1, f False) :| [(1, f True)])
  fmap f (Weighted xs) = Weighted (fmap (second f) xs)


main :: IO ()
main = do
  -- Safe, non-empty list selection
  result1 <- runSelector $ element (1 :| [2, 3, 4, 5])
  print result1

  -- Safe handling of potentially empty lists
  case choose [1, 2, 3] of
    Just selector -> do
      result2 <- runSelector selector
      print result2
    Nothing -> putStrLn "List was empty"

  -- Weighted selection
  case wchoose [(0.3, "Red"), (0.5, "Blue"), (0.2, "Green")] of
    Just selector -> do
      result3 <- runSelector selector
      print result3
    Nothing -> putStrLn "List was empty"

--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--
--module Selector
--  ( Selector
--  , runSelector
--  , element
--  , boolean
--  , weighted
--  , choose
--  , wchoose
--  , wchooseWithDefault
--  ) where
--
--import Control.Monad (replicateM)
--import Data.Kind (Type)
--import Data.List.NonEmpty (NonEmpty(..))
--import qualified Data.List.NonEmpty as NE
--import System.Random.TF (TFGen, newTFGen, mkTFGen)
--import System.Random (randomR, Random)
--import Control.Applicative (pure)
--import Data.Maybe (fromMaybe)
--import Data.Bifunctor (second)
--
--data Selector :: Type -> Type where
--  Pure :: a -> Selector a
--  Element :: NonEmpty a -> Selector a
--  Boolean :: Selector Bool
--  Weighted :: NonEmpty (Double, a) -> Selector a
--  Sequence :: Selector a -> (a -> Selector b) -> Selector b
--
--instance Functor Selector where
--  fmap f (Pure x) = Pure (f x)
--  fmap f (Element xs) = Element (fmap f xs)
--  fmap f Boolean = Sequence Boolean (Pure . f)
--  fmap f (Weighted xs) = Weighted (fmap (second f) xs)
--  fmap f (Sequence m k) = Sequence m (fmap f . k)
--
--instance Applicative Selector where
--  pure = Pure
--  (<*>) = ap
--
--instance Monad Selector where
--  (>>=) = Sequence
--
--runSelector :: Selector a -> IO a
--runSelector s = do
--  gen <- newTFGen
--  pure $ evalSelector gen s
--
--evalSelector :: TFGen -> Selector a -> a
--evalSelector gen s = case s of
--  Pure x -> x
--  Element xs ->
--    let (index, _) = randomR (0, length xs - 1) gen
--    in NE.toList xs !! index
--  Boolean ->
--    let (b, _) = randomR (False, True) gen
--    in b
--  Weighted xs ->
--    let totalWeight = sum (fmap fst xs)
--        (r, _) = randomR (0 :: Double, totalWeight) gen
--        go acc ((w, x):rest)
--          | r <= acc + w = x
--          | otherwise = go (acc + w) rest
--        go _ [] = error "Impossible: weighted selection failed"
--    in go 0 (NE.toList xs)
--  Sequence m k -> evalSelector gen2 (k (evalSelector gen1 m))
--    where (gen1, gen2) = split gen
--
--element :: NonEmpty a -> Selector a
--element = Element
--
--choose :: [a] -> IO (Maybe a)
--choose [] = pure Nothing
--choose (x:xs) = fmap Just $ runSelector $ Element (x :| xs)
--
--wchoose :: [(Double, a)] -> IO (Maybe a)
--wchoose [] = pure Nothing
--wchoose ((w,x):wxs) = Just <$> runSelector (Weighted ((w,x) :| wxs))
--
--wchooseWithDefault :: a -> [(Double, a)] -> IO a
--wchooseWithDefault defaultValue weights = fromMaybe defaultValue <$> wchoose weights
--
--boolean :: Selector Bool
--boolean = Boolean
--
--weighted :: NonEmpty (Double, a) -> Selector a
--weighted = Weighted
--
--split :: TFGen -> (TFGen, TFGen)
--split gen =
--  let (i, gen') = randomR (minBound :: Int, maxBound :: Int) gen
--  in (mkTFGen i, gen')
--
--ap :: Selector (a -> b) -> Selector a -> Selector b
--ap sf sx = do
--  f <- sf
--  f <$> sx









-- MARKOV


--
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE RankNTypes #-}
--
--module Markov where
--
--import Selector
--import Data.List.NonEmpty (fromList)
--import Control.Monad (replicateM)
--
--markov :: String -> Selector String
--markov "A" = weighted (fromList [(0.5, "A"), (0.3, "B"), (0.2, "C")])
--markov "B" = weighted (fromList [(0.4, "A"), (0.4, "B"), (0.2, "C")])
--markov "C" = weighted (fromList [(0.1, "A"), (0.4, "B"), (0.5, "C")])
--markov _   = pure "A"  -- Default to "A" if we encounter an invalid state (for safety)
--
--runMarkovChain :: String -> Int -> Selector [String]
--runMarkovChain start steps = replicateM steps (markov start >>= markov)
--
--main :: IO ()
--main = do
--  result <- runSelector $ runMarkovChain "A" 10
--  putStrLn $ "Markov chain result: " ++ show result
