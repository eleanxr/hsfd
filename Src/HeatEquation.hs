module HeatEquation where

import FiniteDifference
import qualified Data.Vector as V

heatOperator :: UpdateFunction
heatOperator d state i = state V.! i + alpha * coeff * derivative
    where
        alpha = 1.0
        coeff = (timeDelta d) / ((spaceDelta d) * (timeDelta d))
        derivative = state V.! (i + 1) - 2 * state V.! i + state V.! (i - 1)

testState :: V.Vector Double
testState = V.singleton 1.0 V.++ V.replicate 18 0.0 V.++ V.singleton 1.0
