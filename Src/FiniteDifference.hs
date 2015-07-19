module FiniteDifference where

import Data.Array

nextState :: (Num a, Ix b) => (Array b a -> b -> a) -> Array b a -> Array b a
nextState f currentState = undefined
