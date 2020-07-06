Using solitude for anomaly detection
====================================

-   Srikanth Komala Sheshachala

------------------------------------------------------------------------

Introduction
------------

The R package implements **Isolation forest**, an anomaly detection
method introduced by the paper Isolation based Anomaly Detection ([Liu,
Ting and Zhou](https://dl.acm.org/citation.cfm?id=2133363)). Isolation
forest is grown using
[ranger](https://cran.r-project.org/package=ranger) package.

‘Isolation forest’ is a multivariate outlier detection technique for IID
data containing a mix of numeric and categorical variables.

Usage
-----

    library("solitude")

    data("humus", package = "mvoutlier")
    columns_required = setdiff(colnames(humus)
                               , c("Cond", "ID", "XCOO", "YCOO", "LOI")
                               )
    humus2 = humus[ , columns_required]
    str(humus2)

    ## 'data.frame':    617 obs. of  39 variables:
    ##  $ Ag: num  0.124 0.13 0.124 0.309 0.04 0.421 0.235 0.043 0.07 0.086 ...
    ##  $ Al: num  1420 7190 1970 6360 630 2060 1200 1510 3380 1320 ...
    ##  $ As: num  0.743 0.973 1.1 1.24 0.533 1.54 0.836 0.657 1.75 1.04 ...
    ##  $ B : num  2.51 3.45 1.62 1.59 4.33 1.93 2.51 4.46 5.08 2.1 ...
    ##  $ Ba: num  74 65.2 121 63.5 36 111 109 24.5 42.2 51.4 ...
    ##  $ Be: num  0.02 0.08 0.04 0.07 0.01 0.07 0.01 0.05 0.06 0.01 ...
    ##  $ Bi: num  0.108 0.084 0.137 0.135 0.076 0.176 0.145 0.075 0.259 0.112 ...
    ##  $ Ca: num  3900 1900 3240 2470 3500 3340 3010 3090 1760 2810 ...
    ##  $ Cd: num  0.298 0.39 0.295 0.222 0.214 0.327 0.294 0.18 0.295 0.284 ...
    ##  $ Co: num  1.28 3.37 0.98 3.91 0.43 4.61 0.75 0.55 1.68 1.12 ...
    ##  $ Cr: num  3.04 2.02 1.33 17 0.79 3.78 2.01 1.63 10.9 1.71 ...
    ##  $ Cu: num  10.9 12.3 5.59 29.8 6.23 47.3 8.66 5.55 10 9.25 ...
    ##  $ Fe: num  2050 3170 1100 6180 790 3370 1150 1800 4820 970 ...
    ##  $ Hg: num  0.155 0.263 0.168 0.175 0.197 0.239 0.233 0.217 0.238 0.229 ...
    ##  $ K : num  1100 900 1000 900 700 1200 1000 600 800 1000 ...
    ##  $ La: num  2.3 6.4 1.7 5.3 1.2 7 1.5 2.9 6.3 1.2 ...
    ##  $ Mg: num  820 670 790 1180 1770 800 660 1750 820 570 ...
    ##  $ Mn: num  202 22.8 70.3 118 24.4 86.1 1210 16.7 39.2 115 ...
    ##  $ Mo: num  0.662 0.286 0.166 0.28 0.248 0.473 0.185 0.372 0.277 0.15 ...
    ##  $ Na: num  40 120 30 150 410 60 10 320 110 50 ...
    ##  $ Ni: num  9.41 14.8 5.05 59.5 1.98 78 5.7 2.19 11.7 13.2 ...
    ##  $ P : num  743 1030 922 717 795 942 1120 790 779 745 ...
    ##  $ Pb: num  15.9 13.9 19 16.3 14.3 21.7 15.2 11.6 284 14.1 ...
    ##  $ Rb: num  8.14 2.82 4.45 8.13 2.67 5.23 4.94 2.33 4.07 5.35 ...
    ##  $ S : num  1300 1950 1750 965 1860 1410 1880 2210 1260 1250 ...
    ##  $ Sb: num  0.12 0.161 0.25 0.017 0.169 0.217 0.189 0.126 0.809 0.202 ...
    ##  $ Sc: num  0.6 1.2 0.4 1.4 0.2 0.7 0.4 0.6 1.1 0.3 ...
    ##  $ Si: num  630 640 580 690 440 530 650 490 630 560 ...
    ##  $ Sr: num  22.2 34.4 43.4 29.2 46.4 67.9 20.7 59.1 28.8 23.2 ...
    ##  $ Th: num  0.413 0.281 0.246 0.816 0.25 0.432 0.371 0.535 1.13 0.109 ...
    ##  $ Tl: num  0.081 0.068 0.077 0.099 0.06 0.084 0.088 0.042 0.084 0.097 ...
    ##  $ U : num  0.24 0.12 0.042 0.163 0.127 0.112 0.051 0.265 0.221 0.031 ...
    ##  $ V : num  6.79 3.89 2.86 16.1 1.63 7.4 3.55 2.57 13.9 2.76 ...
    ##  $ Y : num  0.8 2.4 0.5 1.9 0.8 1.6 0.5 1.8 1.7 0.4 ...
    ##  $ Zn: num  59.1 18.1 67.6 43.3 41.4 46 103 21.5 23.2 38.5 ...
    ##  $ C : num  39.9 47.5 44.2 19.4 45.8 47.8 45.7 47.3 29.5 46.8 ...
    ##  $ H : num  5.5 6.8 6.3 3 6.1 5.8 6.5 6.4 4.4 6.5 ...
    ##  $ N : num  1.2 1.5 1.5 0.7 1.5 1.3 1.7 1.5 0.7 1 ...
    ##  $ pH: num  3.9 4.1 3.8 4 4.1 3.7 4 4.2 4 3.6 ...

    set.seed(1)
    index = sample(ceiling(nrow(humus2) * 0.5))

    # initiate an isolation forest
    iso = isolationForest$new(sample_size = length(index))

    # fit for attrition data
    iso$fit(humus2[index, ])

    ## INFO  [23:24:08.666] Building Isolation Forest ...  
    ## INFO  [23:24:08.876] done 
    ## INFO  [23:24:08.885] Computing depth of terminal nodes ...  
    ## INFO  [23:24:09.300] done 
    ## INFO  [23:24:09.373] Completed growing isolation forest

    # Obtain anomaly scores
    scores_train = iso$predict(humus2[index, ])
    scores_train[order(anomaly_score, decreasing = TRUE)]

    ##       id average_depth anomaly_score
    ##   1: 169          4.99     0.7220550
    ##   2: 309          6.87     0.6386842
    ##   3: 167          7.21     0.6246686
    ##   4: 103          7.75     0.6030379
    ##   5: 106          8.15     0.5874996
    ##  ---                                
    ## 305:  81         21.70     0.2426414
    ## 306: 175         21.73     0.2421669
    ## 307: 112         21.75     0.2418510
    ## 308: 155         21.88     0.2398078
    ## 309: 174         22.00     0.2379371

    # predict scores for unseen data (50% sample)
    scores_unseen = iso$predict(humus2[-index, ])
    scores_unseen[order(anomaly_score, decreasing = TRUE)]

    ##       id average_depth anomaly_score
    ##   1: 178          7.17     0.6263014
    ##   2: 260          8.61     0.5701249
    ##   3: 271          8.81     0.5627318
    ##   4: 306          9.06     0.5536251
    ##   5:  81          9.24     0.5471597
    ##  ---                                
    ## 304: 266         21.70     0.2426414
    ## 305: 282         21.72     0.2423249
    ## 306:  55         22.25     0.2340866
    ## 307:  51         22.41     0.2316550
    ## 308:  10         22.84     0.2252446

Anomaly detection
-----------------

The paper suggests the following: If the score is closer to 1 for a some
observations, they are likely outliers. If the score for all
observations hover around 0.5, there might not be outliers at all.

By observing the quantiles, we might arrive at the a threshold on the
anomaly scores and investigate the outlier suspects.

    # quantiles of anomaly scores
    quantile(scores_unseen$anomaly_score
             , probs = seq(0.5, 1, length.out = 11)
             )

    ##       50%       55%       60%       65%       70%       75%       80%       85% 
    ## 0.3312529 0.3386517 0.3449757 0.3552106 0.3702403 0.3819597 0.4041027 0.4294298 
    ##       90%       95%      100% 
    ## 0.4620689 0.5100618 0.6263014

The understanding of *why is an observation an anomaly* might require a
combination of domain understanding and techniques like lime (Local
Interpretable Model-agnostic Explanations), Rule based systems etc

Installation
------------

    install.packages("solitude")                  # CRAN version
    devtools::install_github("talegari/solitude") # dev version

------------------------------------------------------------------------
