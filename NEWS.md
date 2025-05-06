# TDLM 1.1.1.9000

This is a list of changes made in the development/GitHub version of the package  
between TDLM 1.1.1 (CRAN release 2025-03-06) and the next CRAN release.


* Renamed the organization to `RTDLM`.

* Added the `great_circle` argument to `extract_spatial_information()` allowing
the users to use their own projection for computing Euclidean distances 
(suggested by @PleaseConverge).

* Changed the threshold of `Ecume::ks_test()` used 
[here](https://github.com/EpiVec/TDLM/blob/master/R/utils.R#L764) 
from 0.001 to 0 to match 
the standard definition of the Kolmogorov–Smirnov test (suggested by 
@PleaseConverge).
  
# TDLM 1.1.1

This is a list of changes made between TDLM 1.1.0 (CRAN release 2025-0107) and  
TDLM 1.1.1 (CRAN release 2025-03-06).

* Added error messages related to the Java dependency.

# TDLM 1.1.0

This is a list of changes made between TDLM 1.0.0 (CRAN release 2023-12-19) and  
TDLM 1.1.0 (CRAN release 2025-01-07).

* Added the function `extract_distances()` to compute distances from 
geographical coordinates.

* Updated `documentation` and `vignette`.

* Updated tests.

# TDLM 1.0.0

This is a list of changes made between TDLM 0.1.0 (CRAN release 2023-03-18) and  
TDLM 1.0.0 (CRAN release 2023-12-19).

* Added automated tests (`testthat` + `covr`).

* Fixed a bug in `extract_spatial_information()` (reported by @Pachka, #1).

* Optimized `extract_spatial_information()` for significantly improved 
performance in distance computation (idea proposed by @Pachka).

* Updated the method for identifying the package folder path in 
`extract_opportunities()`, `run_law_model()`, `run_law()`, and `run_model()` 
(issue identified by @Pachka).

# TDLM 0.1.0

First release on CRAN.



