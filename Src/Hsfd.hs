{-# LANGUAGE MultiParamTypeClasses #-}
import System.Environment

import FiniteDifference
import HeatEquation

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
    evolve output d steps heatOperator boundaryStencil state
    hClose output

writeAsString :: Show a => Handle -> V.Vector a -> IO ()
writeAsString handle vector = hPutStrLn handle $ format vector

instance VectorIO Handle IO Double where
    writeVector = writeAsString

format :: Show a => V.Vector a -> String
format = unwords . (map show) . V.toList
