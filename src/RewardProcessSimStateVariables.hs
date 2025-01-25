module RewardProcessSimStateVariables where

import Control.Monad.State.Lazy
import Control.Monad.Writer 
import System.Random
import Text.Show.Functions
import Data.List

-- stochastic reward process logic starts here --

data Status = Running | Ended deriving (Show)

type StateVariables = [Int]

type Time = Float

type Rate = Float

type Delay = Float

data Transition = T { isEnabled :: StateVariables -> Bool
                    , inverseCDF :: Float -> Float
                    , arrive :: StateVariables -> StateVariables 
                    } deriving (Show)

data FiredTransition = FT { delay :: Delay
                          , arrived :: StateVariables
                          } 
                     | End deriving (Show, Eq)

type Process = [Transition]

type Reward = StateVariables -> Float

-- performance variables are defined in "A Unified Approach for Specifying Measures of Performance, Dependability and Performability", Sanders and Meyer
data PerformanceVariableKind = Instantaneous Time | Accumulated Time Time 

data PerformanceVariableValue = Undefined | Done Float | Working Float

data PerformanceVariable = PV {name :: String 
                           , reward :: Reward
                           , kind :: PerformanceVariableKind
                           , value :: PerformanceVariableValue
                           }

instance Show PerformanceVariable where
  show pv = case (value pv) of
    Done float -> show float
    -- for debugging: Undefined  -> show "undefined"
    -- for debugging: Working float -> show "working" ++ show float
    _ -> "NaN"

performanceVariableTime :: PerformanceVariable -> Time
performanceVariableTime pv = case (kind pv) of
  Instantaneous tr -> tr
  Accumulated ltr utr -> utr

maxPerformanceVariablesTime :: [PerformanceVariable] -> Time
maxPerformanceVariablesTime pvs = performanceVariableTime $ 
  foldl (\x y -> if (performanceVariableTime x)>(performanceVariableTime y) then x else y) PV{kind=Instantaneous 0.0} pvs

performanceVariablesName :: [PerformanceVariable] -> String
performanceVariablesName pvs = concat $ intersperse ", " $ map (\pv -> name pv) pvs

data Current = C { proc :: Process
                 , performanceVariables :: [PerformanceVariable]
                 , here :: StateVariables
                 , now  :: Time
                 , randomNumberGenerator :: StdGen -- TODO at the moment only standard number generator is allowed
                 } deriving (Show)

enabled :: Process -> StateVariables -> [Transition]
enabled proc svs = filter (\x -> ((isEnabled x) svs)) proc -- TODO define connetivity lists to enhance efficiency

-- TODO fix fire and earliest: tuple (gen, svs) instead of gen in the previous version of the code

fire :: RandomGen g => (g, StateVariables) -> Transition -> ((g, StateVariables), FiredTransition)
fire (gen, svs) tr = ((gen', svs), FT {delay=d, arrived=svs'}) where
  sampleAndGen = randomR (0.0::Time,1.0::Time) gen
  y = fst sampleAndGen -- y is a sample from the standard uniform distribution U
  gen' = snd sampleAndGen
  d = (inverseCDF tr) y -- inverse probability integral transform: d is a sample of X=CDF^{-1}_X(U), where X is the transition delay
  svs' = (arrive tr) svs

earliest :: RandomGen g => g -> StateVariables -> [Transition] -> (FiredTransition, g)
earliest gen svs enabledList = if firedTransitions == [] 
                               then (End, gen') 
                               else (selectedTransition, gen') 
                               where genAndStateVariablesAndFiredTransitions = mapAccumL fire (gen, svs) enabledList
                                     firedTransitions = snd genAndStateVariablesAndFiredTransitions
                                     selectedTransition = foldl (\x y -> if (delay x)<(delay y) then x else y) FT{delay = 1.0/0.0, arrived = []} firedTransitions
                                     gen' = fst $ fst genAndStateVariablesAndFiredTransitions

step :: Current -> ((Status, String), Current)
step C{proc=proc, performanceVariables=pvs, here=svs, now=t, randomNumberGenerator=gen} = (
  (status, concat $ intersperse ", " $ map show pvs'), C { proc = proc
                                                         , performanceVariables = pvs' 
                                                         , here = svs'
                                                         , now = t'
                                                         , randomNumberGenerator = gen'
                                                         }) 
  where
  trAndGen = earliest gen svs ( enabled proc svs )
  tr = fst trAndGen
  gen' = snd trAndGen
  t' = if tr==End then t else t + (delay tr) -- advance time
  svs' = if tr==End then svs else arrived tr -- change state variables
  pvs' = map updatePerformanceVariable pvs 
  updatePerformanceVariable pv@PV{name=name, reward=r, kind=k, value=v} = case (v, k) of
    (Done f, _)                      -> pv
    (_, Instantaneous inst)          -> PV{ name=name
                                          , reward=r
                                          , kind=k
                                          , value = if tr/=End then
                                                      if t<=inst && inst<t' 
                                                      then Done (r svs) 
                                                      else Undefined
                                                    else Done (r svs) 
                                          }
    (Undefined, Accumulated lb ub)   -> PV{ name=name
                                          , reward=r
                                          , kind=k
                                          , value = if t<=lb && t'<ub 
                                                    then Working ( (t'-lb)*(r svs) ) 
                                                    else if t<=lb && ub<=t' 
                                                    then Done ( (ub-lb)*(r svs) )
                                                    else Undefined
                                          }
    (Working acc, Accumulated lb ub) -> PV{ name=name
                                          , reward=r
                                          , kind=k
                                          , value = if tr/=End then 
                                                      if ub<=t' 
                                                      then Done ( acc+(ub-t)*(r svs) ) 
                                                      else Working ( acc+(t'-t)*(r svs) ) 
                                                    else Done acc
                                          }        


  status = if tr==End || t' > (maxPerformanceVariablesTime pvs) then Ended else Running

-- stochastic reward process logic ends here --

monadicStep :: State Current (Status, String)
monadicStep = state step 

monadicDES = do
  result <- monadicStep
  case result of
    (Running, _) -> monadicDES
    _            -> return result

oneBatch :: IO Current -> IO Current
oneBatch ioCur = do
  cur <- ioCur
  let ((status, str), cur') = runState monadicDES cur
  putStrLn str
  return cur{randomNumberGenerator = randomNumberGenerator cur'}

sim :: Current -> Int -> IO Current
sim cur n = do
  putStrLn $ performanceVariablesName (performanceVariables cur)
  (iterate oneBatch $ return cur) !! n

