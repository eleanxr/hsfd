module FiniteDifference where

import qualified Data.Vector as V

data Discretization = Discretization {
    timeDelta :: Double,
    spaceDelta :: Double
} deriving (Show)

type NumericArray = V.Vector Double

type UpdateFunction = (Discretization -> NumericArray -> Int -> Double -> Double)

{-  Get the next state of the system given the current state. -}
nextState :: Discretization -> UpdateFunction -> NumericArray -> NumericArray
nextState d f state = imapStencil boundaryStencil update identity state where
    update = f d state
    identity _ v = v

{- Apply a function to a vector given a stencil. Where the stencil
evaluates to True, the function will be applied. Where it is False the
falseFunction is applied. -}
imapStencil :: V.Vector Bool -> (Int -> a -> b) -> (Int -> a -> b)
    -> V.Vector a -> V.Vector b
imapStencil stencil t f v = V.imap conditional zipped where
    zipped = V.zip stencil v
    conditional i (s, value)
        | s = t i value
        | otherwise = f i value

boundaryStencil = V.singleton False V.++ V.replicate 18 True V.++ V.singleton False
