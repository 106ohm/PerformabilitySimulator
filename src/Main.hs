module Main where

import Control.Monad.State.Lazy
import System.Random
import RewardProcessSim

import BirthDeath

main :: IO Current
main = sim cur 1000



