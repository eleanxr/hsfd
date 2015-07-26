module FiniteDifference where

import qualified Data.Vector as V

data Discretization = Discretization {
    timeDelta :: Double,
    spaceDelta :: Double
} deriving (Show)

type NumericArray = V.Vector Double

type UpdateFunction = (Discretization -> NumericArray -> Int -> Double -> Double)

nextState :: Discretization -> UpdateFunction -> NumericArray -> NumericArray
nextState d f currentState = imapStencil boundaryStencil (f d currentState) currentState

imapStencil :: V.Vector Bool -> (Int -> a -> a) -> V.Vector a -> V.Vector a
imapStencil stencil f v = V.imap conditionF (V.zip stencil v) where
    conditionF i (s, value)
        | s = f i value
        | otherwise = value

boundaryStencil = V.singleton False V.++ V.replicate 18 True V.++ V.singleton False
