import System.Environment

import FiniteDifference(Discretization(..), NumericArray, nextState)
import HeatEquation(heatOperator, testState)

import Control.Monad
import qualified Data.Vector as V
import Data.List

import System.IO

main :: IO ()
main = do
    let state = testState
    let steps = [0.0, 0.001 .. 1.0] :: [Double]
    let d = Discretization 0.001 0.05
    output <- openFile "out.dat" WriteMode
    evolve output d steps state
    hClose output

{- Evolve the system, outputting as we go.
    FIXME:
    It seems like I should be able to use forM to evolve the system.
    It also seems like I shouldn't have to mix evolution with IO
-}
evolve :: Handle -> Discretization -> [Double] -> NumericArray -> IO ()
evolve out _ [] finalState = hPutStrLn out $ format finalState
evolve out discretization steps state = do
    hPutStrLn out $ format state
    let state' = nextState discretization heatOperator state
    evolve out discretization (tail steps) state'

format :: Show a => V.Vector a -> String
format = unwords . (map show) . V.toList
