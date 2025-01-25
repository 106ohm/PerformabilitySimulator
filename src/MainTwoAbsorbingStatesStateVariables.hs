module Main where

import Control.Monad.State.Lazy
import System.Random
import RewardProcessSimStateVariables

import TwoAbsorbingStatesStateVariables

main :: IO Current
main = sim cur 10



