# PerformabilitySimulator
A simple simulator to evaluate performability measures on stochastic processes                  

# Description
Given a discrete space, continuous time, stochastic process and performance variables (actually, we are interessed in performability variables, i.e., performance and/or dependability variables), each defined through a reward structure as in "A Unified Approach for Specifying Measures of Performance, Dependability, and Performability" (Sanders and Meyer, 1999), this code simulates *nbatches* times the process and evaluates the variables.

# Install
It is suggested to exploit [cabal](https://cabal.readthedocs.io) for cloning PerformabilitySimulator.

# Examples
At the moment we have implemented the following examples:
* BirthDeath: a simple birth-death CTMC
* TwoAbsorbingStates: a simple CTMC with two absorbing states
* ConditionalAccumulatedReward: here the reward structure is non trivial

# Details on _RewardProcessSim_
A _node_ is an integer and represent a _model state_.

A _simulation state_ comprises all the information carried on: transitions and performance variables. In the code, the word "state" means simulation state and is manipolated through the State monad.

A _transition_ represents how the model changes.
A transition is active only in one _start_ node and switch the model to one _arrive_ node.
A transition is activated after a rand delay, drawn from a given probability distribution; in the code, the inverse of the Cumulative Distribution Function is implemented in _inverseCDF_. 

A _reward_ is a list of floats, indexed by nodes.

# Details on _RewardProcessSimStateVariables_
A _state variable_ is an integer. A _model state_ is represented as a list of state variables. In the code, the corresponding type is __StateVariables__.

A _simulation state_ comprises all the information carried on: transitions, state variables and performance variables. In the code, the word "state" means simulation state and is manipolated through the State monad.

A _transition_ is enabled in a subset of model states; in the code, _isEnabled_ is responsible for checking.
If activated, a transition changes the model state; in the code, _arrive_ defines how.
Once enabled, a transition is activated after a rand delay, drawn from a given probability distribution; in the code, the inverse of the Cumulative Distribution Function is implemented in _inverseCDF_. 

A _reward_ if a function that evaluted on a model state returns a float.

# Run
Just run `$cabal run -v0 example_name | tee results.csv`, then you can use your favorite software, e.g., R, to compute mean, variance, higher moments-related values, confidence intervales, etc. 

# Didactics
A few reasons to study and/or exercize PerformabilitySimulator are:
* it is a very simple Discrete Event Simulator, so a good starting point to understand the basic concepts behind the implementation of a DES. In particular, here each state is represented as an integer and, among the enabled transitions, the earliest is selected. Actually, the stochastic process itself is represented as a list of transitions. Of course this is _not_ the most _efficient_ way of selecting the next state given a state, i.e., apply the _step_ function, because each time the list of enabled transition has to be defined and scanned, and is _not_ the most _effective_ because usually this kind of process is defined through an higher level formalism (e.g., Stochastic Petri Nets, Process Algebra, etc) and then the state is beter represented in other ways. Here clareness and simplicity are more relevant than expressibility and performance.
* it is a good example of simple but not trivial application of the _state monad_. Given a state we want to select and transition to another state, updating the performance variables: is there a better application of the state monad?
* also the use of the IO monad is interesting, exploited to run _nbatches_ simulation batches and write on the standard output the performance variables.
* each transition, when fired, produce a delay that is a random variable drawn from a given distribution. As common practice in the field, we exploit the Probability Integral Transform Theorem to sample a given distribution, and it is interesting how the random number generator is managed.
