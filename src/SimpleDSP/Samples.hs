-- | This module contains the data representation of samples.
module SimpleDSP.Samples where

import Data.Vector.Storable (length, Vector)

type Samples =  Data.Vector.Storable.Vector Float

-- | Normalize the current position between 0.0 and 1.0
normalizePos :: Samples -> Int -> Float
normalizePos samples pos = fromIntegral pos / fromIntegral ( Data.Vector.Storable.length samples)
