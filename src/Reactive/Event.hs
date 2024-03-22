{-# LANGUAGE RecursiveDo #-}
{-# HLINT ignore "Use zipWithFrom" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Reactive.Event where

-- Import necessary modules
{- import Pitch.Pitch
import Time.Dur -}

-- Import required functions from standard libraries
import Control.Monad (ap, join)
import Control.Monad.Fix
import Data.Maybe (fromMaybe)
import Data.These (These (..), these)

-- Model implementation for learning and testing.
-- Event and Behavior

-- Define type synonyms
-- Natural numbers (poorly represented)
type Nat = Int

type Time = Nat -- Time starts at t = 0

-- Define newtype for Event holding Maybe values
newtype Event a = E {unE :: [Maybe a]} deriving (Show)

-- Define newtype for Behavior holding values
newtype Behavior a = B {unB :: [a]} deriving (Show)

-- Function to interpret events using moments
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpret f as =
  take (length as) . unE . (\m -> unM m 0) . f . E $ (as <> repeat Nothing)

-- First-order functions

-- Functor instance for Event
instance Functor Event where
  fmap f (E xs) = E (fmap (fmap f) xs)

-- Functor instance for Behavior
instance Functor Behavior where
  fmap f (B xs) = B (fmap f xs)

-- Applicative instance for Behavior
instance Applicative Behavior where
  pure x = B $ repeat x
  (B f) <*> (B x) = B $ zipWith ($) f x

-- Create an event that never occurs
never :: Event a
never = E $ repeat Nothing

-- Combine two events using a combining function
unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith = mergeWith id id

-- Merge two events using custom mapping functions
mergeWith ::
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  Event a ->
  Event b ->
  Event c
mergeWith f g h xs ys = these f g h <$> merge xs ys

-- Merge two events into a single event of These values
merge :: Event a -> Event b -> Event (These a b)
merge (E xs) (E ys) = E $ zipWith combine xs ys
  where
    combine Nothing Nothing = Nothing
    combine (Just x) Nothing = Just (This x)
    combine Nothing (Just y) = Just (That y)
    combine (Just x) (Just y) = Just (These x y)

-- Filter out 'Nothing' values from an event of 'Maybe a'
filterJust :: Event (Maybe a) -> Event a
filterJust = E . fmap join . unE

-- Apply a behavior of functions to an event of values
apply :: Behavior (a -> b) -> Event a -> Event b
apply (B fs) = E . zipWith (\f mx -> fmap f mx) fs . unE

-- Moment and accumulation

-- Define a newtype for Moment representing computations over time
newtype Moment a = M {unM :: Time -> a}

-- Functor instance for Moment
instance Functor Moment where fmap f = M . fmap f . unM

-- Applicative instance for Moment
instance Applicative Moment where
  pure = M . const
  (<*>) = ap

-- Monad instance for Moment
instance Monad Moment where
  return = pure
  (M m) >>= k = M $ \time -> unM (k $ m time) time

-- MonadFix instance for Moment
instance MonadFix Moment where
  mfix f = M $ mfix (unM . f)

-- Forget all event occurrences before a particular time
forgetE :: Time -> Event a -> [Maybe a]
forgetE time (E xs) = drop time xs

-- Create a behavior that holds the last event occurrence at each time step
stepper :: a -> Event a -> Moment (Behavior a)
stepper i e = M $ \time -> B $ replicate time i <> step i (forgetE time e)
  where
    step i ~(x : xs) = i : step next xs
      where
        next = fromMaybe i x

-- Accumulate events over time to produce a new event
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
  E (replicate t Nothing <> switch (unE e) (forgetE t (forgetDiagonalE es)))
  where
    switch (x : xs) (Nothing : ys) = x : switch xs ys
    switch (x : _) (Just xs : ys) = x : switch (tail xs) ys

forgetDiagonalE :: Event (Event a) -> Event [Maybe a]
forgetDiagonalE = E . zipWith (\time -> fmap (forgetE time)) [0 ..] . unE

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB b e = diagonalB <$> stepper b e

diagonalB :: Behavior (Behavior a) -> Behavior a
diagonalB = B . zipWith (\time xs -> xs !! time) [0 ..] . fmap unB . unB
