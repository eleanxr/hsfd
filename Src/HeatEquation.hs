module HeatEquation where

import FiniteDifference
import qualified Data.Vector as V

heatOperator :: UpdateFunction Double
heatOperator d state i _ = state V.! i + alpha * coeff * derivative
    where
        alpha = 1.0
        coeff = (timeDelta d) / (spaceDelta d)**2
        derivative = state V.! (i + 1) - 2 * state V.! i + state V.! (i - 1)

testState :: V.Vector Double
testState = V.singleton 1.0 V.++ V.replicate 18 0.0 V.++ V.singleton 1.0
