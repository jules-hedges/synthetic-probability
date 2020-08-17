{-
This is a demonstration of doing "synthetic" probabilistic programming using the continuation monad.
For documentation, see the blog post
https://julesh.com/2020/08/15/probabilistic-programming-with-continuations/
-}

module SyntheticProbability where

import           Control.Monad.Cont
import           Numeric.Tools.Integration

type Prob = Cont Double

uniformDiscrete :: [x] -> Prob x
uniformDiscrete xs = let n = fromIntegral (length xs)
                      in cont $ \k -> sum (map k xs) / n

probability :: Prob x -> (x -> Bool) -> Double
probability integrate p = runCont integrate $ \x -> if p x then 1 else 0

expectation :: Prob Double -> Double
expectation integrate = runCont integrate id

twoDice :: Prob Int
twoDice = do roll1 <- uniformDiscrete [1..6]
             roll2 <- uniformDiscrete [1..6]
             return (roll1 + roll2)

uniformUnitInterval :: Prob Double
uniformUnitInterval = cont $ \k -> quadBestEst (quadTrapezoid params (0, 1) k)
  where params = QuadParam {quadPrecision = 0.00001, quadMaxIter = 30}
