module HeatEquation where

import Data.Array
import FiniteDifference(Discretization)

heatOperator :: (Ix a, Num b) => Discretization -> Array b a -> b -> a
heatOperator d state i = state!i + alpha * coeff * derivative
    where
        alpha = 1.0
        coeff = (dt d) / ((dx d) * (dx d))
        derivative = state!(i + 1) - 2 * state!i + state!(i - 1)
