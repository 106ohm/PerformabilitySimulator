# PerformabilitySimulator
A simple simulator to evaluate performability measures on stochastic processes                  

# Description
Given a discrete space, continuous time, stochastic process and performance variables (actually, we are interessed in performability variables, i.e., performancd and/or dependability variables), each defined through a reward structure as in "A Unified Approach for Specifying Measures of Performance, Dependability, and Performability" (Sanders and Meyer, 1999), this code simulates *nbatches* times the process and evaluates the variables.

# Install
It is suggested to use [cabal](https://cabal.readthedocs.io) for cloning PerformabilitySimulator.

# Examples
At the moment we have implemented the following examples:
* BirthDeath: a simple birth-death CTMC
* TwoAbsorbingStates: a simple CTMC with two absorbing states
* ConditionalAccumulatedReward: here the reward structure is non trivial

# Run
Just run `$cabal run -v0 example_name | tee results.csv`, then you can use your favorite software, e.g., R, to compute mean, variance, higher moments-related values, confidence intervales, etc. 

# Didactics
I can see a few reasons to study and/or exercize PerformabilitySimulator:
* it is a very simple Discrete Event Simulator, so a good starting point to understand the basic concepts behind the implementation of a DES. In particular, here each state is represented as an integer and, among the enabled transitions, the earliest is selected. Actually, the stochastic process itself is represented as a list of transitions. Of course this is _not_ the most _efficient_ way of selecting the next state given a state, i.e., apply the _step_ function, because each time the list of enabled transition has to be defined and scanned, and is _not_ the most _effective_ because usually this kind of process is defined through an higher level formalism (e.g., Stochastic Petri Nets, Process Algebra, etc) and then the state is beter represented in other ways. Here clareness and simplicity are more relevant than expressibility and performance.
* it is a good example of simple but not trivial application of the _state monad_. Given a state we want to select and transition to another state, updating the performance variables: is there a better application of the state monad?
* also the use of the IO monad is interesting, exploited to run _nbatches_ simulation batches and write on the standard output the performance variables.
* each transition, when fired, produce a delay that is a random variable drawn from a given distribution. As common practice in the field, we exploit the Probability Integral Transform Theorem to sample a given distribution, and it is interesting how the random number generator is managed.
