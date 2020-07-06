# solitude 1.0.1

- finalized on the API
- isolationForest object no longer calculates and stores anomaly scores on training data. Use predict method for this.
- mtry is no longer exposed
- version 1.0.0 was a faulty release, do not use it

# solitude 0.2.1

- added 'mtry' argument for isolationForest method
- minor optimizations in 'terminalNodesDepth' function

# solitude 0.2.0

- Changes in API: `isolationForest` is now a R6 class with new(), fit() and predict() methods.
- Exported `terminalNodesDepth` function to obtain the depth of terminal nodes of all trees in a ranger forest. 

# solitude 0.1.3

- Changed defaults to avoid bagging in extremely randomized trees (see https://stats.stackexchange.com/questions/406666/what-exactly-is-the-extratrees-option-in-ranger)

- Using `future_lapply` in favor of `furrr::furrr_map`
