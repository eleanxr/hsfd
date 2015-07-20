module FiniteDifference where

import Data.Array

data Discretization = Discretization {
    dt :: Double,
    dx :: Double
}

nextState :: (Num a, Ix b) => 
    Discretization -> (Discretization -> Array b a -> b -> a) -> Array b a -> Array b a
nextState d f currentState = array bounds' nextState'
    where
        bounds' = bounds currentState
        nextState' = map (\i -> (i, f d currentState i)) (indices currentState)
