------------------------------------------------------------------------

Introduction
------------

The R package implements **Isolation forest**, an anomaly detection
method introduced by the paper Isolation based Anomaly Detection ([Liu,
Ting and Zhou](https://dl.acm.org/citation.cfm?id=2133363)).

Isolation forest is grown using
[ranger](https://cran.r-project.org/package=ranger) package and it is
possible to **experiment with the variants** of classical isolation
forest ex: weighing covariates(features) and observations.

Usage
-----

    suppressPackageStartupMessages(library("dplyr"))
    suppressPackageStartupMessages(library("solitude"))
    data("Boston", package = "MASS")
    dplyr::glimpse(Boston)

    ## Observations: 506
    ## Variables: 14
    ## $ crim    <dbl> 0.00632, 0.02731, 0.02729, 0.03237, 0.06905, 0.02985, 0.…
    ## $ zn      <dbl> 18.0, 0.0, 0.0, 0.0, 0.0, 0.0, 12.5, 12.5, 12.5, 12.5, 1…
    ## $ indus   <dbl> 2.31, 7.07, 7.07, 2.18, 2.18, 2.18, 7.87, 7.87, 7.87, 7.…
    ## $ chas    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ nox     <dbl> 0.538, 0.469, 0.469, 0.458, 0.458, 0.458, 0.524, 0.524, …
    ## $ rm      <dbl> 6.575, 6.421, 7.185, 6.998, 7.147, 6.430, 6.012, 6.172, …
    ## $ age     <dbl> 65.2, 78.9, 61.1, 45.8, 54.2, 58.7, 66.6, 96.1, 100.0, 8…
    ## $ dis     <dbl> 4.0900, 4.9671, 4.9671, 6.0622, 6.0622, 6.0622, 5.5605, …
    ## $ rad     <int> 1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4,…
    ## $ tax     <dbl> 296, 242, 242, 222, 222, 222, 311, 311, 311, 311, 311, 3…
    ## $ ptratio <dbl> 15.3, 17.8, 17.8, 18.7, 18.7, 18.7, 15.2, 15.2, 15.2, 15…
    ## $ black   <dbl> 396.90, 396.90, 392.83, 394.63, 396.90, 394.12, 395.60, …
    ## $ lstat   <dbl> 4.98, 9.14, 4.03, 2.94, 5.33, 5.21, 12.43, 19.15, 29.93,…
    ## $ medv    <dbl> 24.0, 21.6, 34.7, 33.4, 36.2, 28.7, 22.9, 27.1, 16.5, 18…

    BostonX <- Boston %>% select(-medv)

    # grow an isolation forest
    iso_Boston <- isolationForest(BostonX, seed = 100, num.trees = 1e3)

    # predict anomaly scores (parallelizable using futures)
    scores <- predict(iso_Boston, BostonX, type = "anomaly_score")

    # predict corrected depths
    depths <- predict(iso_Boston, BostonX, type = "depth_corrected")

Anomaly detection
-----------------

The paper suggests the following: If the score is closer to 1 for a some
observations, they are likely outliers. If the score for all
observations hover around 0.5, there might not be outliers at all.

By observing the quantiles, we might arrive at the a threshold on the
anomaly scores and investigate the outlier suspects.

    # quantiles of anomaly scores
    quantile(scores, probs = seq(0.5, 1, length.out = 11))

    ##       50%       55%       60%       65%       70%       75%       80% 
    ## 0.4403705 0.4480122 0.4550305 0.4608977 0.4696051 0.4814371 0.4884172 
    ##       85%       90%       95%      100% 
    ## 0.4914260 0.5184716 0.5288953 0.6552715

The understanding of *why is an observation an anomaly* might require a
combination of domain understanding and techniques like lime (Local
Interpretable Model-agnostic Explanations), Rule based systems etc

Installation
------------

    install.packages("solitude")                  # CRAN version
    devtools::install_github("talegari/solitude") # dev version

------------------------------------------------------------------------
