module TwoAbsorbingStatesStateVariables where

import RewardProcessSimStateVariables
import System.Random

-- We would like to evaluate the Mean Time To Failure conditioned to been absorbed in "1".
-- To do that, we define Y with r that is equal to 1 on all the transient states (in this case only [0]) and 0 on the absorbing states [1] and [2],
-- so Y is the time to absorbtion, and V the indicator function on state [1].
-- Thus, the MTTF conditioned to the event "X=1" can be computed as E[Y|V=1]

invCDFexp :: Float -> Float
invCDFexp y = -log(1-y)/1.0

isEnabled1 :: StateVariables -> Bool
isEnabled1 svs = if (svs!!0 == 0)
                 then True
		 else False

isEnabled2 :: StateVariables -> Bool
isEnabled2 = isEnabled1

arrive1 :: StateVariables -> StateVariables
arrive1 svs = [1]

arrive2 :: StateVariables -> StateVariables
arrive2 svs = [2]

rewardY :: StateVariables -> Float
rewardY svs = if (svs!!0 == 0)
                 then 1.0
		 else 0.0

rewardV :: StateVariables -> Float
rewardV svs = if (svs!!0 == 1)
                 then 1.0
		 else 0.0

cur = C { proc = [ A{isEnabled=isEnabled1,inverseCDF=invCDFexp,arrive=arrive1}
                 , A{isEnabled=isEnabled2,inverseCDF=invCDFexp,arrive=arrive2}
                 ]
        , performanceVariables = [ PV { name = "Y"
                                      , reward = rewardY -- this is the reward of interest, nonzero only on transient states
                                      , kind = Accumulated 0.0 20.0
                                      , value = Undefined 
                                      }
                                 , PV { name = "V"
                                      , reward = rewardV -- this identifies the absorbing state of interest
                                      , kind = Instantaneous 20.0
                                      , value = Undefined 
                                      } 
                                 ]
        , here=[0]
        , now=0.0
        , randomNumberGenerator=mkStdGen 2
        }
