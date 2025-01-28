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

data Action = A { isEnabled :: StateVariables -> Bool
                , inverseCDF :: Float -> Float
                , arrive :: StateVariables -> StateVariables 
                } deriving (Show)

data FiredAction = FA { delay :: Delay
                      , arrived :: StateVariables
                      } 
                 | End deriving (Show, Eq)

type Process = [Action]

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

enabled :: Process -> StateVariables -> [Action]
enabled proc svs = filter (\x -> ((isEnabled x) svs)) proc -- TODO define connetivity lists to enhance efficiency

fire :: RandomGen g => (g, StateVariables) -> Action -> ((g, StateVariables), FiredAction)
fire (gen, svs) a = ((gen', svs), FA {delay=d, arrived=svs'}) where
  sampleAndGen = randomR (0.0::Time,1.0::Time) gen
  y = fst sampleAndGen -- y is a sample from the standard uniform distribution U
  gen' = snd sampleAndGen
  d = (inverseCDF a) y -- inverse probability integral transform: d is a sample of X=CDF^{-1}_X(U), where X is the transition delay
  svs' = (arrive a) svs

earliest :: RandomGen g => g -> StateVariables -> [Action] -> (FiredAction, g)
earliest gen svs enabledList = if firedActions == [] 
                               then (End, gen') 
                               else (selectedAction, gen') 
                               where genAndStateVariablesAndFiredActions = mapAccumL fire (gen, svs) enabledList
                                     firedActions = snd genAndStateVariablesAndFiredActions
                                     selectedAction = foldl (\x y -> if (delay x)<(delay y) then x else y) FA {delay = 1.0/0.0, arrived = []} firedActions
                                     gen' = fst $ fst genAndStateVariablesAndFiredActions

step :: Current -> ((Status, String), Current)
step C{proc=proc, performanceVariables=pvs, here=svs, now=t, randomNumberGenerator=gen} = (
  (status, concat $ intersperse ", " $ map show pvs'), C { proc = proc
                                                         , performanceVariables = pvs' 
                                                         , here = svs'
                                                         , now = t'
                                                         , randomNumberGenerator = gen'
                                                         }) 
  where
  aAndGen = earliest gen svs ( enabled proc svs )
  a = fst aAndGen
  gen' = snd aAndGen
  t' = if a==End then t else t + (delay a) -- advance time
  svs' = if a==End then svs else arrived a -- change state variables
  pvs' = map updatePerformanceVariable pvs 
  updatePerformanceVariable pv@PV{name=name, reward=r, kind=k, value=v} = case (v, k) of
    (Done f, _)                      -> pv
    (_, Instantaneous inst)          -> PV { name=name
                                           , reward=r
                                           , kind=k
                                           , value = if a/=End then
                                                       if t<=inst && inst<t' 
                                                       then Done (r svs) 
                                                       else Undefined
                                                     else Done (r svs) 
                                           }
    (Undefined, Accumulated lb ub)   -> PV { name=name
                                           , reward=r
                                           , kind=k
                                           , value = if t<=lb && t'<ub 
                                                     then Working ( (t'-lb)*(r svs) ) 
                                                     else if t<=lb && ub<=t' 
                                                     then Done ( (ub-lb)*(r svs) )
                                                     else Undefined
                                           }
    (Working acc, Accumulated lb ub) -> PV { name=name
                                           , reward=r
                                           , kind=k
                                           , value = if a/=End then 
                                                       if ub<=t' 
                                                       then Done ( acc+(ub-t)*(r svs) ) 
                                                       else Working ( acc+(t'-t)*(r svs) ) 
                                                     else Done acc
                                           }        


  status = if a==End || t' > (maxPerformanceVariablesTime pvs) then Ended else Running

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

