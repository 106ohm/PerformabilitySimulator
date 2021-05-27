module TwoAbsorbingStates where

import RewardProcessSim
import System.Random

-- We would like to evaluate E[Y|X=2], to do that we evaluate E[Y|V=1] and then with R
-- we perform the empirical mean of only those Y where V is equal to 1. 
-- In this example the absorbing states are 1 and 3

invCDFexp :: Float -> Float
invCDFexp y = -log(1-y)/1.0

cur = C { proc = [ T{start=0,inverseCDF=invCDFexp,arrive=1}
                 , T{start=0,inverseCDF=invCDFexp,arrive=2}
                 , T{start=2,inverseCDF=invCDFexp,arrive=3}
                 ]
        , performanceVariables = [ PV { name = "Y"
                                      , reward = [1, 0, 1, 0] -- this is the reward of interest, nonzero only on transient states
                                      , kind = Accumulated 0.0 20.0
                                      , value = Undefined 
                                      }
                                 , PV { name = "V"
                                      , reward = [0, 1, 0, 0] -- this identifies the absorbing state of interest
                                      , kind = Instantaneous 20.0
                                      , value = Undefined 
                                      } 
                                 ]
        , here=0
        , now=0.0
        , randomNumberGenerator=mkStdGen 2
        }
