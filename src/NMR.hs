module NMR where

import RewardProcessSimStateVariables
import System.Random
import Data.List

-- parameters
n = 3
m = 2 -- ceiling ( (n+1)/2 )
maxSimTime = 100

-- N Modular Redundancy
-- there are n state variables, one for each component.
-- each state variable is 1 if the corresponding component is OK, 0 if is KO
-- as long as a simple majority of components are OK the system is OK, otherwise the system fails,
-- and the simulation stops

-- replace the k-th element of a given list
replaceKth :: Int -> a -> [a] -> [a]
replaceKth _ _ [] = []
replaceKth k newVal (x:xs)
  | k == 0 = newVal:xs
  | otherwise = x:replaceKth (k-1) newVal xs

-- example: slice 0 3 [0,1,2,3,4,5] will produces [0,1,2,3]
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- initial state variables
initialStateVariables = replicate n 1

-- probability related definitions
invCDFexp :: Float -> Float
invCDFexp y = -log(1-y)/1.0

-- model components' fault through unfoldr
componentFault :: Int -> Maybe (Action, Int)
componentFault k = if k >= n
                   then Nothing
		   else Just ( A { isEnabled = \svs -> if sum svs >= m && svs!!k == 1 then True else False
		                 , inverseCDF = invCDFexp
			         , arrive = \svs -> replaceKth k 0 svs
				 }
			     , k+1)


-- reward structures
rewardY :: StateVariables -> Float
rewardY svs = if sum svs >= m then 1.0 else 0.0

-- assembly model and performance variables
cur = C { proc = unfoldr componentFault 0
        , performanceVariables = [ PV { name = "Y"
                                      , reward = rewardY 
                                      , kind = Accumulated 0.0 maxSimTime
                                      , value = Undefined 
                                      }
                                 ]
        , here = initialStateVariables
        , now = 0.0
        , randomNumberGenerator=mkStdGen 2
        }
