module SimpleDSP.Samples where

import Data.Vector.Storable (Vector, length)

type Samples = Data.Vector.Storable.Vector Float

normalizePos :: Samples -> Int -> Float
normalizePos samples pos = fromIntegral pos / fromIntegral (Data.Vector.Storable.length samples)
