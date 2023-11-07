module Time.Dur
  ( Dur (..),
    (%/),
    HasDur (..),
    normalizeDurList,
    (|/),
    (|*),
    dur,
  )
where

import Data.Default
import Data.Ratio

-- | The 'Dur' type represents musical durations or any other kind of durations
-- as a rational number.
newtype Dur = Dur
  { unDur :: Rational
  }
  deriving (Eq, Ord, Num, Fractional, Real)

instance Show Dur where
  show (Dur x) = "Dur (" ++ show x ++ ")"

-- | Default value for 'Dur'
-- >>> def :: Dur
-- Dur (1 % 1)
instance Default Dur where
  def = Dur (1 % 1)

-- | Operator to construct a 'Dur' value from two integral values.
-- It allows for more concise and readable creation of 'Dur' values from
-- simple numeric literals.
--
-- __Examples__
--
-- >>> 2 %/ 3
-- Dur (2 % 3)
--
-- >>> 4 %/ 6
-- NOW Dur (2 % 3)
infix 7 %/

(%/) :: Integer -> Integer -> Dur
x %/ y = dur (x % y)

-- | Creates a 'Dur' value from any 'Real' number by converting it to
-- a 'Rational'. This is a convenience function to quickly turn floating-point
-- numbers into 'Dur' values without needing to deal with rationals directly.
--
-- >>> dur 0.5
-- Dur (1 % 2)
--
-- >>> dur (1%3)
-- Dur (1 % 3)
dur :: (Real a) => a  -> Dur
dur = Dur . toRational 

-- | The 'HasDur' class is used for types that can be converted to and from 'Dur',
-- a representation of duration as a rational number. This allows for abstraction
-- over different representations of duration.
--
-- For instance, an 'Integer' can be th ought of as a duration in some unit, and
-- 'Rational' can represent a fraction of that unit.
--
-- Implementing 'HasDur' for a type indicates that it has a meaningful conversion
-- to and from 'Dur'.
class HasDur a where
  -- \| Convert a value to its 'Dur' representation.

  toDur :: a -> Dur

  -- \| Convert a 'Dur' back to its original representation. Provide a default

  -- 'fromDur' that assumes 'a' is 'Dur' itself or can be naturally
  -- represented as a 'Rational'.
  fromDur :: Dur -> a
  default fromDur :: (a ~ Rational) => Dur -> a
  fromDur (Dur r) = r

-- | 'Dur' values are trivially converted to themselves.
instance HasDur Dur where
  -- \| The identity function for 'Dur', as 'Dur' values are already in the correct form.

  toDur = id

  -- \| The identity function for 'Dur', since no conversion is necessary.
  fromDur = id

-- | Rationals can represent durations directly, so they can be easily wrapped in or extracted from 'Dur'.
instance HasDur Rational where
  -- \| Wrap a 'Rational' number in a 'Dur'.

  toDur = Dur

  -- \| Unwrap a 'Dur' to get the underlying 'Rational' number.
  fromDur = unDur

-- | Integers can be thought of as durations in whole units, so they are converted
-- to 'Dur' by treating them as such.
instance HasDur Integer where
  -- \| Convert an 'Integer' to a 'Dur' by considering the 'Integer' as a whole number
  -- duration, thus having a denominator of 1.

  toDur x = Dur (x % 1)

  -- \| Convert a 'Dur' back to an 'Integer'. This will take the numerator of the 'Dur',
  -- effectively truncating the duration to a whole number.
  -- Note: This may result in loss of information if the 'Dur' represents a fractional duration.
  fromDur = numerator . unDur

------------------------------
----  Utilitity Functions ----
------------------------------

-- | Normalizes a list of values that represent durations of time to a list of
-- 'Dur' values that sum to 1.
--
-- >>> normalizeDurList [1, 2, 3]
-- [Dur (1 % 6),Dur (1 % 3),Dur (1 % 2)]
normalizeDurList :: (HasDur a) => [a] -> [Dur]
normalizeDurList durations = [toDur x / toDur total | x <- durations]
  where
    total = sum $ map (unDur . toDur) durations

-- | Divides each value in a list of durations by a 'Real' value. 
-- FIXME 
-- >>> [Dur (1), Dur(1/2), Dur(1/3)]  |/ 2  
-- WAS NOW [Dur (1 % 2),Dur (1 % 4),Dur (1 % 6)]
-- NOW [Dur (1 % 2),Dur (1 % 4),Dur (1 % 6)]
(|/) :: (HasDur a, Real b) => [a] -> b -> [a]
durations |/ divisorValue =
  map
    ( \durVal ->
        let durRational = toRational (toDur durVal)
            divisor = toRational divisorValue
         in fromDur $ Dur (durRational / divisor)
    )
    durations

-- | Multiplies each value in a list of durations by a 'Real' value. Works with
-- both 'Integer' and 'Rational' as divisor values.
--
-- >>> [Dur (1), Dur(1/2), Dur(1/3)] |* 2
-- NOW [Dur (2 % 1),Dur (1 % 1),Dur (2 % 3)]
(|*) :: (HasDur a, Real b) => [a] -> b -> [a]
durations |* multiplierValue =
  map
    ( \durVal ->
        let durRational = toRational (toDur durVal)
            multiplier = toRational multiplierValue
         in fromDur $ Dur (durRational * multiplier)
    )
    durations

--------------

-- | Test values.
-- >>> from_dur_1
-- >>> from_dur_2
-- 3 % 4
-- Dur (3 % 4)
from_dur_1 :: Rational
from_dur_1 = fromDur $ Dur (3 % 4)

from_dur_2 :: Dur
from_dur_2 = fromDur $ Dur (3 % 4)
