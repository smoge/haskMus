module Pitch.Scale where 


import Data.Ratio
import Pitch.Pitch
import Util.Fraction
import Data.List (intercalate)


newtype Interval = Interval { getInterval :: Rational } deriving (Eq, Ord)

instance Show Interval where
  show :: Interval -> String
  show (Interval i) = show i


data Scale = Scale
    { scaleName :: String
    , scaleIntervals :: [Interval]
    , scaleMode :: Maybe Int  -- 0 for root position, 1 for first mode, etc.
}


instance Show Scale where
  show (Scale name intervals mode) = 
    name <> " [" <> intercalate "," (fmap (show . getInterval) intervals) <> "]" <> 
    maybe "" (\m -> " (Mode: " <> show m <> ")") mode

majorScale :: Scale
majorScale = Scale "Major" [Interval 2, Interval 2, Interval 1, Interval 2, Interval 2, Interval 2, Interval 1] Nothing

minorScale :: Scale
minorScale = Scale "Minor" [Interval 2, Interval 1, Interval 2, Interval 2, Interval 1, Interval 2, Interval 2] Nothing



newtype Sieve = Sieve { getSieve :: Integer -> Bool }

simpleSieve :: Integer -> Integer -> Sieve
simpleSieve modulus residue = Sieve (\n -> n `mod` modulus == residue)

unionSieve :: Sieve -> Sieve -> Sieve
unionSieve (Sieve s1) (Sieve s2) = Sieve (\n -> s1 n || s2 n)

intersectSieve :: Sieve -> Sieve -> Sieve
intersectSieve (Sieve s1) (Sieve s2) = Sieve (\n -> s1 n && s2 n)

complementSieve :: Sieve -> Sieve
complementSieve (Sieve s) = Sieve (not . s)

xenakisSieve :: Sieve
xenakisSieve = unionSieve (simpleSieve 3 0) (intersectSieve (simpleSieve 2 0) (simpleSieve 5 0))

scaleFromSieve :: String -> Sieve -> Integer -> Scale
scaleFromSieve name sieve range =
  Scale name (fmap (Interval . toRational) intervals) Nothing
  where
    intervals = filter (getSieve sieve) [0..range]


-- >>> scaleFromSieve "xen" xenakisSieve 14
-- xen [0 % 1,3 % 1,6 % 1,9 % 1,10 % 1,12 % 1]


