Solitude
========

------------------------------------------------------------------------

Introduction
------------

The R package implements **Isolation forest**, an anomaly detection
method introduced by the paper Isolation based Anomaly Detection ([Liu,
Ting and Zhou](https://dl.acm.org/citation.cfm?id=2133363)).

Isolation forest is grown using
[ranger](https://cran.r-project.org/package=ranger) package.

Usage
-----

    suppressPackageStartupMessages(library("solitude"))
    data("attrition", package = "rsample")
    str(attrition)

    ## 'data.frame':    1470 obs. of  31 variables:
    ##  $ Age                     : int  41 49 37 33 27 32 59 30 38 36 ...
    ##  $ Attrition               : Factor w/ 2 levels "No","Yes": 2 1 2 1 1 1 1 1 1 1 ...
    ##  $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 2 3 2 3 2 3 3 2 3 ...
    ##  $ DailyRate               : int  1102 279 1373 1392 591 1005 1324 1358 216 1299 ...
    ##  $ Department              : Factor w/ 3 levels "Human_Resources",..: 3 2 2 2 2 2 2 2 2 2 ...
    ##  $ DistanceFromHome        : int  1 8 2 3 2 2 3 24 23 27 ...
    ##  $ Education               : Ord.factor w/ 5 levels "Below_College"<..: 2 1 2 4 1 2 3 1 3 3 ...
    ##  $ EducationField          : Factor w/ 6 levels "Human_Resources",..: 2 2 5 2 4 2 4 2 2 4 ...
    ##  $ EnvironmentSatisfaction : Ord.factor w/ 4 levels "Low"<"Medium"<..: 2 3 4 4 1 4 3 4 4 3 ...
    ##  $ Gender                  : Factor w/ 2 levels "Female","Male": 1 2 2 1 2 2 1 2 2 2 ...
    ##  $ HourlyRate              : int  94 61 92 56 40 79 81 67 44 94 ...
    ##  $ JobInvolvement          : Ord.factor w/ 4 levels "Low"<"Medium"<..: 3 2 2 3 3 3 4 3 2 3 ...
    ##  $ JobLevel                : int  2 2 1 1 1 1 1 1 3 2 ...
    ##  $ JobRole                 : Factor w/ 9 levels "Healthcare_Representative",..: 8 7 3 7 3 3 3 3 5 1 ...
    ##  $ JobSatisfaction         : Ord.factor w/ 4 levels "Low"<"Medium"<..: 4 2 3 3 2 4 1 3 3 3 ...
    ##  $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 3 2 3 2 2 3 2 1 3 2 ...
    ##  $ MonthlyIncome           : int  5993 5130 2090 2909 3468 3068 2670 2693 9526 5237 ...
    ##  $ MonthlyRate             : int  19479 24907 2396 23159 16632 11864 9964 13335 8787 16577 ...
    ##  $ NumCompaniesWorked      : int  8 1 6 1 9 0 4 1 0 6 ...
    ##  $ OverTime                : Factor w/ 2 levels "No","Yes": 2 1 2 2 1 1 2 1 1 1 ...
    ##  $ PercentSalaryHike       : int  11 23 15 11 12 13 20 22 21 13 ...
    ##  $ PerformanceRating       : Ord.factor w/ 4 levels "Low"<"Good"<"Excellent"<..: 3 4 3 3 3 3 4 4 4 3 ...
    ##  $ RelationshipSatisfaction: Ord.factor w/ 4 levels "Low"<"Medium"<..: 1 4 2 3 4 3 1 2 2 2 ...
    ##  $ StockOptionLevel        : int  0 1 0 0 1 0 3 1 0 2 ...
    ##  $ TotalWorkingYears       : int  8 10 7 8 6 8 12 1 10 17 ...
    ##  $ TrainingTimesLastYear   : int  0 3 3 3 3 2 3 2 2 3 ...
    ##  $ WorkLifeBalance         : Ord.factor w/ 4 levels "Bad"<"Good"<"Better"<..: 1 3 3 3 3 2 2 3 3 2 ...
    ##  $ YearsAtCompany          : int  6 10 0 8 2 7 1 1 9 7 ...
    ##  $ YearsInCurrentRole      : int  4 7 0 7 2 7 0 0 7 7 ...
    ##  $ YearsSinceLastPromotion : int  0 1 0 3 2 3 0 0 1 7 ...
    ##  $ YearsWithCurrManager    : int  5 7 0 0 2 6 0 0 8 7 ...

    attritionX <- attrition[, setdiff(colnames(attrition), "Attrition")]

    # initiate an isolation forest
    iso <- isolationForest$new()

    # fit for attrition data
    iso$fit(attritionX)

    ## Building Isolation Forest ... done
    ## Computing depth of terminal nodes ... done

    # Obtain anomaly scores
    print(iso$scores)

    ##         id average_depth anomaly_score
    ##    1:    1         13.39     0.5089184
    ##    2:    2         13.16     0.5148575
    ##    3:    3         13.45     0.5073803
    ##    4:    4         14.78     0.4744556
    ##    5:    5         14.56     0.4797505
    ##   ---                                 
    ## 1466: 1466         15.08     0.4673294
    ## 1467: 1467         14.96     0.4701670
    ## 1468: 1468         13.99     0.4937455
    ## 1469: 1469         16.91     0.4261191
    ## 1470: 1470         13.81     0.4982492

Anomaly detection
-----------------

The paper suggests the following: If the score is closer to 1 for a some
observations, they are likely outliers. If the score for all
observations hover around 0.5, there might not be outliers at all.

By observing the quantiles, we might arrive at the a threshold on the
anomaly scores and investigate the outlier suspects.

    # quantiles of anomaly scores
    quantile(iso$scores$anomaly_score
             , probs = seq(0.5, 1, length.out = 11)
             )

    ##       50%       55%       60%       65%       70%       75%       80% 
    ## 0.4916329 0.4969816 0.5022870 0.5071245 0.5135605 0.5216548 0.5304650 
    ##       85%       90%       95%      100% 
    ## 0.5387713 0.5480918 0.5659473 0.6357167

The understanding of *why is an observation an anomaly* might require a
combination of domain understanding and techniques like lime (Local
Interpretable Model-agnostic Explanations), Rule based systems etc

Installation
------------

    install.packages("solitude")                  # CRAN version
    devtools::install_github("talegari/solitude") # dev version

------------------------------------------------------------------------
