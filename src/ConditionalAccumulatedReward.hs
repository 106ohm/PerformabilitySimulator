module ConditionalAccumulatedReward where

import RewardProcessSim
import System.Random

-- We would like to evaluate the Mean accumulated reward conditioned to been absorbed in "3".
-- To do that, we define Y with r that nonzero only on the transient states, and V the indicator function on state "3".
-- Thus, the mean of Y conditioned to the event "X=3" can be computed as E[Y|V=1]
-- In this example the absorbing states are 2 and 3

invCDFexp1 :: Float -> Float
invCDFexp1 y = -log(1-y)/1.0

invCDFexp2 :: Float -> Float
invCDFexp2 y = -log(1-y)/2.0

invCDFexp3 :: Float -> Float
invCDFexp3 y = -log(1-y)/3.0

cur = C { proc = [ T{start=0,inverseCDF=invCDFexp3,arrive=3}
                 , T{start=0,inverseCDF=invCDFexp1,arrive=1}
                 , T{start=1,inverseCDF=invCDFexp2,arrive=2}
                 ]
        , performanceVariables = [ PV { name = "Y"
                                      , reward = [12, 7, 0, 0] -- this is the reward of interest, nonzero only on transient states
                                      , kind = Accumulated 0.0 20.0
                                      , value = Undefined 
                                      }
                                 , PV { name = "V"
                                      , reward = [0, 0, 0, 1] -- this identifies the absorbing state of interest
                                      , kind = Instantaneous 20.0
                                      , value = Undefined 
                                      } 
                                 ]
        , here=0
        , now=0.0
        , randomNumberGenerator=mkStdGen 2
        }
