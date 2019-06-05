# solitude 0.1.3

- Changed defaults to avoid bagging in extremly randomized trees (see https://stats.stackexchange.com/questions/406666/what-exactly-is-the-extratrees-option-in-ranger)

- Using `future_lapply` in favour of `furrr::furrr_map`
