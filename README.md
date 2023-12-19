# __TDLM__ <img src="man/figures/logo.png" align="right" alt="" width="200" />

# Systematic comparison of trip distribution laws and models 

<!-- badges: start -->
[![R-CMD-check](https://github.com/EpiVec/TDLM/workflows/R-CMD-check/badge.svg)](https://github.com/EpiVec/TDLM/actions)
[![Codecov test coverage](https://codecov.io/gh/EpiVec/TDLM/branch/master/graph/badge.svg)](https://app.codecov.io/gh/EpiVec/TDLM?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/TDLM)](https://cran.r-project.org/package=TDLM)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/TDLM)](https://r-pkg.org:443/pkg/TDLM)  
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05434/status.svg)](https://doi.org/10.21105/joss.05434)
<!-- badges: end -->

## 1 Short description

The main purpose of the `TDLM`'s package is to propose a rigorous framework to 
fairly compare trip distribution laws and models as described in 
[Lenormand et al. (2016)](https://www.sciencedirect.com/science/article/abs/pii/S0966692315002422). 
This general framework is based on a two-step approach to generate mobility flows
by separating the trip distribution law, gravity or intervening opportunities, from 
the modeling approach used to generate the flows from this law.

## 2 Install

The package `TDLM` can be installed with the following command line in R session:

From the CRAN

``` r
install.packages("TDLM")
```

or from GitHub

``` r
# install.packages("devtools")
devtools::install_github("EpiVec/TDLM", build_vignettes = TRUE)
```

## 3 Tutorial

A tutorial vignette is available [here](https://epivec.github.io/TDLM/articles/TDLM.html).

## 4 Dependencies

`TDLM` depends on `Ecume`, `mathjaxr`, `Rdpack`, `readr`, `rmarkdown` and `sf`.

## 5  Citation

Lenormand M (2023) [TDLM: An R package for a systematic comparison of
trip distribution laws and models.](https://joss.theoj.org/papers/10.21105/joss.05434#) *Journal of Open Source Software* 8, 5434.

## 6 Contribution instructions

Please feel free to open an issue if you encounter a problem
with the package.

You can also reach me at maxime.lenormand[at]inrae.fr.

