{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FiniteDifference where

import qualified Data.Vector as V

data Discretization = Discretization {
    timeDelta :: Double,
    spaceDelta :: Double
} deriving (Show)

type UpdateFunction  a = (Discretization -> V.Vector a -> Int -> a -> a)

{-|  Get the next state of the system given the current state. -}
nextState :: Discretization -> UpdateFunction a -> V.Vector a -> V.Vector a
nextState d f state = imapStencil boundaryStencil update identity state where
    update = f d state
    identity _ v = v

{-| Apply a function to a vector given a stencil. Where the stencil
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

{-| Allow customization of vector IO. -}
class Monad m => VectorIO h m a | m -> h where
    writeVector :: h -> V.Vector a -> m ()

{-| Evolve the given operator through a set of timesteps, processing each state
using a VectorIO instance.
-}
evolve :: VectorIO h m a => h -> Discretization -> [Double]
    -> UpdateFunction a -> V.Vector a -> m ()
evolve out _ [] _ finalState = writeVector out finalState
evolve out discretization steps operator state = do
    writeVector out state
    evolve out discretization (tail steps) operator state'
        where state' = nextState discretization operator state
