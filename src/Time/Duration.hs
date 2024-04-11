{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Time.Duration where

import Control.Lens
import Data.Data (Data)
import Data.List (sortOn)
import Data.Ord (comparing)
import Data.Ratio
import Language.Haskell.TH.Syntax
import Util.MathDuration
import Prelude hiding (toRational)

newtype Division = Division {unDivision :: Integer}
  deriving (Eq, Show, Ord, Data, Lift)

newtype Dots = Dots {unDot :: Integer}
  deriving (Eq, Show, Enum, Ord, Data, Lift)

newtype Multiplier = Multiplier {unMultiplier :: Rational}
  deriving (Eq, Show, Ord, Data, Lift)

data Duration where
  Duration ::
    { _division :: Division,
      _dots :: Dots,
      _multiplier :: Rational
    } ->
    Duration
  deriving (Eq, Show, Data, Lift)

makeLenses ''Duration

-- | Convert a 'Duration' to Lilypond notation
durationToLilypond :: Duration -> String
durationToLilypond (Duration (Division 0) dts _) =
  "\\breve" <> replicate (fromIntegral $ unDot dts) '.'
durationToLilypond (Duration dv dts _) =
  show (unDivision dv) <> replicate (fromIntegral $ unDot dts) '.'

-- | Calculate the multiplier for a given number of dots
dotMultiplier :: Dots -> Rational
dotMultiplier (Dots d) = 1 + (2 ^ d - 1) % (2 ^ d)

-- | Order a list of 'Rational' numbers based on their musical simplicity
orderByMusicalSimplicity :: [Rational] -> [Rational]
orderByMusicalSimplicity = sortOn musicalOrderHelper

-- | Add a specified number of dots to a 'Duration'
addDotsToDuration :: Duration -> Integer -> Duration
addDotsToDuration dur m = dur & dots .~ newDots
  where
    newDots = Dots (unDot (dur ^. dots) + m)

infixl 6 +.

infixl 6 -.

-- | Operator for adding dots to a 'Duration'
(+.) :: Duration -> Integer -> Duration
d +. i = addDotsToDuration d i

-- | Operator for subtracting dots from a 'Duration'
(-.) :: Duration -> Integer -> Duration
d -. i = addDotsToDuration d (negate i)

-- | Custom 'Ord' instance for 'Duration'
instance Ord Duration where
  -- Compare two durations based on their 'Rational' representation
  compare = comparing durationToRational

-- | Convert a 'Duration' to a 'Rational'
durationToRational :: Duration -> Rational
durationToRational (Duration (Division divVal) dots_ m)
  | divVal == 0 = 0 % 1
  | otherwise = (1 % divVal) * dotMultiplier dots_ * m

-- | Convert a 'Division' to a 'Rational'
divisionToRational :: Division -> Rational
divisionToRational (Division 0) = 0 % 1
divisionToRational (Division d) = 1 % d

-- | Convert a value to a 'Rational'
class ToRational a where
  toRational :: a -> Rational

instance ToRational Rational where
  toRational = id

instance ToRational Division where
  toRational (Division 0) = 0 % 1
  toRational (Division d) = 1 % d

-- | Convert a 'Duration' to a 'Rational'
durationToRat :: Duration -> Rational
durationToRat (Duration (Division divVal) dots_ m)
  | divVal == 0 = 0 % 1
  | otherwise = (1 % divVal) * dotMultiplier dots_ * m

-- | Get the number of dots corresponding to a given multiplier
dotsFromMultiplier :: Rational -> Maybe Dots
dotsFromMultiplier r
  | r < 0 = Nothing -- check for negative rationals
  | otherwise = binarySearch 0 9
  where
    -- Cache for dotMultiplier, converting each integer to Dots
    cache = fmap (dotMultiplier . Dots) [0 .. 9]

    binarySearch :: Integer -> Integer -> Maybe Dots
    binarySearch low high
      | low > high = Nothing
      | midMultiplier == r = Just $ Dots mid
      | midMultiplier < r = binarySearch (mid + 1) high
      | otherwise = binarySearch low (mid - 1)
      where
        mid = (low + high) `div` 2
        midMultiplier = cache !! fromIntegral mid

-- | Get the number of dots corresponding to a given multiplier
-- >>>  dotsFromMultiplier' (3 % 2)
-- Dots {unDot = 1}
-- >>>  dotsFromMultiplier' (7 % 4)
-- Dots {unDot = 2}
-- >>>  dotsFromMultiplier' (15 % 8)
-- Dots {unDot = 3}
-- >>>  dotsFromMultiplier' (31 % 16)
-- Dots {unDot = 4}
-- >>>  dotsFromMultiplier' (63 % 32)
-- Dots {unDot = 5}
-- >>>  dotsFromMultiplier' (127 % 64)
-- Dots {unDot = 6}
-- >>>  dotsFromMultiplier' (255 % 128)
-- Dots {unDot = 7}
-- >>>  dotsFromMultiplier' (511 % 256)
-- Dots {unDot = 8}
-- >>>  dotsFromMultiplier' (1023 % 512)
-- Dots {unDot = 9}
dotsFromMultiplier' :: Rational -> Dots
dotsFromMultiplier' r = binarySearch 0 maxDots
  where
    maxDots = 20 -- Define an appropriate upper limit based on requirements

    binarySearch :: Integer -> Integer -> Dots
    binarySearch low high
      | low > high = error "Multiplier not found within range"
      | midMultiplier == r = Dots mid
      | midMultiplier < r = binarySearch (mid + 1) high
      | otherwise = binarySearch low (mid - 1)
      where
        mid = (low + high) `div` 2
        midMultiplier = dotMultiplier (Dots mid)

{- dotMultiplier :: Dots -> Rational
dotMultiplier (Dots d) = -- Implement the logic to calculate multiplier based on number of dots
 -}

-- | Check if two durations have the same multiplier
isMEQ :: Duration -> Duration -> Bool
isMEQ d1 d2 = d1 ^. multiplier == d2 ^. multiplier

-- | Check if a duration has a multiplier of 1
isM1 :: Duration -> Bool
isM1 d = d ^. multiplier == 1

-- | Check if a duration has no dots
noDots :: Duration -> Bool
noDots d = unDot (d ^. dots) == 0

-- | Create a 'Dots' value from an integer
mkDots :: Integer -> Dots
mkDots n = Dots (abs n)
