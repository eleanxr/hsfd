import System.Environment

import FiniteDifference(Discretization(..), NumericArray, nextState)
import HeatEquation(heatOperator, testState)

import Control.Monad
import qualified Data.Vector as V
import Data.List

main :: IO ()
main = do
    let state = testState
    let steps = [0.0, 0.001 .. 20.0] :: [Double]
    let d = Discretization 0.001 0.05
    evolve d steps state

{- Evolve the system, outputting as we go.
It seems like I should be able to use forM to evolve the system.
-}
evolve :: Discretization -> [Double] -> NumericArray -> IO ()
evolve _ [] finalState = print $ format finalState
evolve discretization steps state = do
    print $ format state
    let state' = nextState discretization heatOperator state
    evolve discretization (tail steps) state'

format :: Show a => V.Vector a -> String
format v = intercalate " " (map show (V.toList v))
