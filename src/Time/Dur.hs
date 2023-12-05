{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Time.Dur (
    Dur (..),
    (%/),
    HasDur (..),
    normalizeDurList,
) where

import Control.Lens
import Data.Data (Data)
import Data.Default
import Data.Ratio

newtype Dur = Dur
    { unDur :: Rational
    }
    deriving (Eq, Ord, Num, Fractional, Real, Data)

makeLenses ''Dur

instance Show Dur where
    show :: Dur -> String
    show (Dur x) = "Dur (" <> show x <> ")"

-- instance Show Dur where
--   showsPrec d (Dur x) = showParen (d > 10) $ showString "Dur " . showsPrec 11 x

{- | Default value for 'Dur'
 >>> def :: Dur
 Dur (1 % 1)
-}
instance Default Dur where
    def = Dur (1 % 1)

infix 7 %/

(%/) :: Integer -> Integer -> Dur
x %/ y = dur (x % y)

dur :: Rational -> Dur
dur = Dur

class HasDur a where
    toDur :: a -> Dur
    setDur :: a -> Dur -> a

instance HasDur Dur where
    toDur = id
    setDur a _ = Dur (unDur a)

normalizeDurList :: (HasDur a) => [a] -> [Dur]
normalizeDurList durations = fmap (\x -> toDur x / toDur total) durations
  where
    total = sum $ fmap toDur durations

-- !FIX
-- (|/) :: (HasDur a, Real b) => [a] -> b -> [a]
-- durations |/ divisor =
--     fmap (\durVal -> let temdur = toDur durVal
--                          divisorRat = toRational divisor
--                          newDur = Dur (unDur temdur / divisorRat)
--                     in setDuration dur durVal newDur)
--          durations

-- (|*) :: (HasDur a, Real b) => [a] -> b -> [a]
-- durations |* multiplier =
--     fmap (\durVal -> let temdur = toDur durVal
--                          multiplierRat = toRational multiplier
--                          newDur = Dur (unDur temdur * multiplierRat)
--                     in setDuration dur durVal newDur)
--          durations

-- setDuration :: ASetter a a Dur Dur -> a -> Dur -> a
-- setDuration durSetter hasDur newDur = set durSetter newDur hasDur
