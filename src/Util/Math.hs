module Until.Math where

import Data.List (genericLength)
import Text.Printf (printf)

chebyshevTanCoeffs :: Int -> [Double]
chebyshevTanCoeffs n =
  let n' = fromIntegral n
      pi' = pi :: Double
      coeffs =
        [ sum [tan (x_k k) * cos (fromIntegral j * acos (x_k k)) | k <- [1 .. n]]
          | j <- [0 .. n - 1]
        ]
      x_k k = cos (pi' * (fromIntegral k - 0.5) / n')
   in fmap ((2.0 / n') *) coeffs

chebyshevTanApprox :: [Double] -> Double -> Double
chebyshevTanApprox coeffs x =
  let n = length coeffs
      result = head coeffs / 2 + sum [coeffs !! i * cos (fromIntegral i * acos x) | i <- [1 .. n - 1]]
   in result

-- chebyshevTanCoeffs 10
-- [8.881784197001253e-17,1.3800431431142994,3.1086244689504386e-16,0.1546028061638477,7.105427357601002e-16,1.9821882628706124e-2,1.1546319456101628e-15,2.5538677244514307e-3,1.7097434579227411e-15,2.879200910730129e-4

compareTanApprox :: Int -> IO ()
compareTanApprox n = do
  let coeffs = chebyshevTanCoeffs n
  putStrLn $ "Coefficients for N = " ++ show n ++ ":"
  mapM_ (\(i, c) -> printf "c_%d = %.15e\n" i c) $ zip [0 ..] coeffs

  putStrLn "\nComparison of approximation vs actual tan:"
  let testPoints = [0.1, 0.2 .. 0.9]
  let results = map (\x -> (x, chebyshevTanApprox coeffs x, tan x)) testPoints
  mapM_
    ( \(x, approx, actual) ->
        printf
          "x = %.2f: Approx = %.6f, Actual = %.6f, Diff = %.6e\n"
          x
          approx
          actual
          (abs (approx - actual))
    )
    results

main :: IO ()
main = do
  putStrLn "Enter the degree of approximation (N):"
  n <- readLn
  compareTanApprox n

-- >main
-- Enter the degree of approximation (N):
-- 10
-- Coefficients for N = 10:
-- c_0 = 8.881784197001253e-17
-- c_1 = 1.380043143114299e0
-- c_2 = 3.108624468950439e-16
-- c_3 = 1.546028061638477e-1
-- c_4 = 7.105427357601002e-16
-- c_5 = 1.982188262870612e-2
-- c_6 = 1.154631945610163e-15
-- c_7 = 2.553867724451431e-3
-- c_8 = 1.709743457922741e-15
-- c_9 = 2.879200910730129e-4
--
-- Comparison of approximation vs actual tan:
-- x = 0.10: Approx = 0.100338, Actual = 0.100335, Diff = 3.156938e-6
-- x = 0.20: Approx = 0.202705, Actual = 0.202710, Diff = 5.081993e-6
-- x = 0.30: Approx = 0.309318, Actual = 0.309336, Diff = 1.808463e-5
-- x = 0.40: Approx = 0.422779, Actual = 0.422793, Diff = 1.403397e-5
-- x = 0.50: Approx = 0.546319, Actual = 0.546302, Diff = 1.623064e-5
-- x = 0.60: Approx = 0.684177, Actual = 0.684137, Diff = 4.051529e-5
-- x = 0.70: Approx = 0.842293, Actual = 0.842288, Diff = 5.088008e-6
-- x = 0.80: Approx = 1.029576, Actual = 1.029639, Diff = 6.229755e-5
-- x = 0.90: Approx = 1.260174, Actual = 1.260158, Diff = 1.569295e-5
