# solitude

An implementation of Isolation forest: Anomaly detection method introduced by the paper Isolation based Anomaly Detection (Liu, Ting and Zhou 10.1145/2133360.2133363>)

- Function 'isolation_forest' builds the isolation forest with optional arguments passed to [ranger](https://cran.r-project.org/package=future).

- 'predict' method provides one of the these outputs: anomaly_score, depth_corrected. This can be computed in parallel using an appropriate [future](https://cran.r-project.org/package=future).

----
Srikanth Komala Sheshachala
