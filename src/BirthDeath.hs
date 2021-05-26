module BirthDeath where

import RewardProcessSim
import System.Random

nstates = 5

r = reverse (map fromIntegral [0..nstates-1] :: [Float])

invCDFexp :: Float -> Float
invCDFexp y = -log(1-y)/1.0

cur = C { proc = ( map (\i -> T{start=i,inverseCDF=invCDFexp,arrive=i+1}) [0..nstates-2] ) 
                 ++
                 ( map (\i -> T{start=i+1,inverseCDF=invCDFexp,arrive=i}) $ reverse [0..nstates-2] )
        , performanceVariables = [ PV { name = "acc"
                                      , reward = r
                                      , kind = Accumulated 0.0 20.0
                                      , value = Undefined 
                                      }
                                 , PV { name = "ins"
                                      , reward = r
                                      , kind = Instantaneous 10.0
                                      , value = Undefined 
                                      } 
                                 ]
        , here=0
        , now=0.0
        , randomNumberGenerator=mkStdGen 2
        }
