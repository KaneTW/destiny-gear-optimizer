# Destiny Gear Optimizer
## Install from source
1. Install Stack -- https://haskellstack.org
2. Clone or download this project
3. `stack build`
4. `stack exec destiny-gear-optimizer-exe`
5. Follow instructions.

## Theory
This models Destiny loot as a stochastic dynamic programming problem.

Each activity is encoded by the possible power gain, how often you can run it, and the possible loot. We assume a uniform distribution over all slots, without any extra weight (you can change that if you'd like in `defaultActions` in Destiny.Drops).

A gear state consists of deviations of gear from the mean, bounded automatically by summing to less than 8 (otherwise the mean would be higher). This severely reduces the gear state space, allowing for easier computation.

An action state is a n-tuple of remaining actions. Each entry index corresponds to an entry in `defaultActions`.

This gives us a total state (gear state, action state), from which we can determine the optimal course of action and the expected total gain as a result.

