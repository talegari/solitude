# solitude 0.2.0

- Changes in API: `isolationForest` is now a R6 class with new(), fit() and predict() methods.
- Exported `terminalNodesDepth` function to obtain the depth of terminal nodes of all trees in a ranger forest. 

# solitude 0.1.3

- Changed defaults to avoid bagging in extremly randomized trees (see https://stats.stackexchange.com/questions/406666/what-exactly-is-the-extratrees-option-in-ranger)

- Using `future_lapply` in favour of `furrr::furrr_map`
