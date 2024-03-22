{-# LANGUAGE RecursiveDo #-}
{-# HLINT ignore "Use zipWithFrom" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Reactive.Event where

{- import Pitch.Pitch
import Time.Dur -}
import Control.Monad
import Control.Monad.Fix
import Data.Maybe (fromMaybe)
import Data.These (These (..), these)

-- \| Model implementation for learning and testing.
--    Event and Behavior

-- | Natural numbers (poorly represented).
type Nat = Int

type Time = Nat -- begins at t = 0

newtype Event a = E {unE :: [Maybe a]} deriving (Show)

newtype Behavior a = B {unB :: [a]} deriving (Show)

interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpret f as =
  take (length as) . unE . (\m -> unM m 0) . f . E $ (as <> repeat Nothing)

--    First-order

instance Functor Event where
  fmap f (E xs) = E (fmap (fmap f) xs)

instance Functor Behavior where
  fmap f (B xs) = B (fmap f xs)

instance Applicative Behavior where
  pure x = B $ repeat x
  (B f) <*> (B x) = B $ zipWith ($) f x

never :: Event a
never = E $ repeat Nothing

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith = mergeWith id id

mergeWith ::
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  Event a ->
  Event b ->
  Event c
mergeWith f g h xs ys = these f g h <$> merge xs ys

merge :: Event a -> Event b -> Event (These a b)
merge (E xs) (E ys) = E $ zipWith combine xs ys
  where
    combine Nothing Nothing = Nothing
    combine (Just x) Nothing = Just (This x)
    combine Nothing (Just y) = Just (That y)
    combine (Just x) (Just y) = Just (These x y)

filterJust :: Event (Maybe a) -> Event a
filterJust = E . fmap join . unE

apply :: Behavior (a -> b) -> Event a -> Event b
apply (B fs) = E . zipWith (\f mx -> fmap f mx) fs . unE

--    Moment and accumulation
newtype Moment a = M {unM :: Time -> a}

instance Functor Moment where fmap f = M . fmap f . unM

instance Applicative Moment where
  pure = M . const
  (<*>) = ap

instance Monad Moment where
  return = pure
  (M m) >>= k = M $ \time -> unM (k $ m time) time

instance MonadFix Moment where
  mfix f = M $ mfix (unM . f)

-- Forget all event occurences before a particular time
forgetE :: Time -> Event a -> [Maybe a]
forgetE time (E xs) = drop time xs

stepper :: a -> Event a -> Moment (Behavior a)
stepper i e = M $ \time -> B $ replicate time i <> step i (forgetE time e)
  where
    step i ~(x : xs) = i : step next xs
      where
        next = fromMaybe i x

-- Expressed using recursion and the other primitives
-- FIXME: Strictness!
accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE a e1 = mdo
  let e2 = ((\a f -> f a) <$> b) `apply` e1
  b <- stepper a e2
  pure e2

--    Higher-order

valueB :: Behavior a -> Moment a
valueB (B b) = M $ \time -> b !! time

observeE :: Event (Moment a) -> Event a
observeE = E . zipWith (\time -> fmap (\m -> unM m time)) [0 ..] . unE

switchE :: Event a -> Event (Event a) -> Moment (Event a)
switchE e es = M $ \t ->
  E $
    (replicate t Nothing <> switch (unE e) (forgetE t (forgetDiagonalE es)))
  where
    switch (x : xs) (Nothing : ys) = x : switch xs ys
    switch (x : _) (Just xs : ys) = x : switch (tail xs) ys

forgetDiagonalE :: Event (Event a) -> Event [Maybe a]
forgetDiagonalE = E . zipWith (\time -> fmap (forgetE time)) [0 ..] . unE

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB b e = diagonalB <$> stepper b e

diagonalB :: Behavior (Behavior a) -> Behavior a
diagonalB = B . zipWith (\time xs -> xs !! time) [0 ..] . fmap unB . unB
