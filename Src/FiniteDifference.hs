module FiniteDifference where

import qualified Data.Vector as V

data Discretization = Discretization {
    timeDelta :: Double,
    spaceDelta :: Double
}

type NumericArray = V.Vector Double

type UpdateFunction = (Discretization -> NumericArray -> Int -> Double)

nextState :: Discretization -> UpdateFunction -> NumericArray -> NumericArray
nextState d f currentState = V.imap updateElement (V.slice 1 elementCount currentState)
    where
        updateElement = \e _ -> f d currentState e
        elementCount = V.length currentState - 2

imapStencil :: V.Vector Bool -> (Int -> a -> a) -> V.Vector a -> V.Vector a
imapStencil stencil f v = V.imap conditionF (V.zip stencil v) where
    conditionF i (s, value)
        | s = f i value
        | otherwise = value
