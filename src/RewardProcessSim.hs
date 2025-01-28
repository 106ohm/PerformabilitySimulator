module RewardProcessSim where

import Control.Monad.State.Lazy
import Control.Monad.Writer 
import System.Random
import Text.Show.Functions
import Data.List

-- stochastic reward process logic starts here --

data Status = Running | Ended deriving (Show)

type Node = Int

type Time = Float

type Rate = Float

type Delay = Float

data Transition = T { start :: Node
                    , inverseCDF :: Float -> Float
                    , arrive :: Node 
                    } deriving (Show)

data FiredTransition = FT { delay :: Delay
                          , arrived :: Node
                          } 
                     | End deriving (Show, Eq)

type Process = [Transition]

type Reward = [Float]

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
                 , here :: Node
                 , now  :: Time
                 , randomNumberGenerator :: StdGen -- TODO at the moment only standard number generator is allowed
                 } deriving (Show)

enabled :: Process -> Node -> [Transition]
enabled proc n = filter (\x -> start x == n) proc -- TODO define connetivity lists to enhance efficiency

fire :: RandomGen g => g -> Transition -> (g, FiredTransition)
fire gen tr = (gen', FT {delay=d, arrived=n'}) where
  sampleAndGen = randomR (0.0::Time,1.0::Time) gen
  y = fst sampleAndGen -- y is a sample from the standard uniform distribution U
  gen' = snd sampleAndGen
  d = (inverseCDF tr) y -- inverse probability integral transform: d is a sample of X=CDF^{-1}_X(U), where X is the transition delay
  n' = arrive tr

earliest :: RandomGen g => [Transition] -> g -> (FiredTransition, g)
earliest enabledList gen = if firedTransitions == [] 
                           then (End, gen') 
                           else (selectedTransition , gen') 
                           where genAndFiredTransitions = mapAccumL fire gen enabledList
                                 firedTransitions = snd genAndFiredTransitions
                                 selectedTransition = foldl (\x y -> if (delay x)<(delay y) then x else y) FT {delay = 1.0/0.0, arrived = -1} firedTransitions
                                 gen' = fst genAndFiredTransitions

step :: Current -> ((Status, String), Current)
step C{proc=proc, performanceVariables=pvs, here=n, now=t, randomNumberGenerator=gen} = (
  (status, concat $ intersperse ", " $ map show pvs'), C { proc = proc
                                                         , performanceVariables = pvs' 
                                                         , here = n'
                                                         , now = t'
                                                         , randomNumberGenerator = gen'
                                                         }) 
  where
  trAndGen = earliest ( enabled proc n ) gen
  tr = fst trAndGen
  gen' = snd trAndGen
  t' = if tr==End then t else t + (delay tr) -- advance time
  n' = if tr==End then n else arrived tr -- change node
  pvs' = map updatePerformanceVariable pvs 
  updatePerformanceVariable pv@PV{name=name, reward=r, kind=k, value=v} = case (v, k) of
    (Done f, _)                      -> pv
    (_, Instantaneous inst)          -> PV { name=name
                                           , reward=r
                                           , kind=k
                                           , value = if tr/=End then
                                                       if t<=inst && inst<t' 
                                                       then Done (r !! n) 
                                                       else Undefined
                                                     else Done (r !! n)
                                           }
    (Undefined, Accumulated lb ub)   -> PV { name=name
                                           , reward=r
                                           , kind=k
                                           , value = if t<=lb && t'<ub 
                                                     then Working ( (t'-lb)*(r !! n) ) 
                                                     else if t<=lb && ub<=t' 
                                                     then Done ( (ub-lb)*(r !! n) )
                                                     else Undefined
                                           }
    (Working acc, Accumulated lb ub) -> PV { name=name
                                           , reward=r
                                           , kind=k
                                           , value = if tr/=End then 
                                                       if ub<=t' 
                                                       then Done ( acc+(ub-t)*(r !! n) ) 
                                                       else Working ( acc+(t'-t)*(r !! n) )
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

-- TODO write on file

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

