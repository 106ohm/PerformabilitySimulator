module Main where

import Control.Monad.State.Lazy
import System.Random
import RewardProcessSim

import TwoAbsorbingStates

main :: IO Current
main = sim cur 10000



