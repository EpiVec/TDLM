# __TDLM__ <img src="man/figures/logo.png" align="right" alt="" width="138" />

# Systematic comparison of trip distribution laws and models 

<!-- badges: start -->
[![R-CMD-check](https://github.com/RTDLM/TDLM/workflows/R-CMD-check/badge.svg)](https://github.com/RTDLM/TDLM/actions)
[![Codecov test coverage](https://codecov.io/gh/RTDLM/TDLM/branch/master/graph/badge.svg)](https://app.codecov.io/gh/RTDLM/TDLM?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/TDLM)](https://cran.r-project.org/package=TDLM)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/TDLM)](https://r-pkg.org:443/pkg/TDLM)  
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05434/status.svg)](https://doi.org/10.21105/joss.05434)  
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14979715.svg)](https://doi.org/10.5281/zenodo.14979715)
<!-- badges: end -->

## 1 Short description

The main purpose of the `TDLM` package is to provide a rigorous framework for 
fairly comparing trip distribution laws and models, as described in 
[Lenormand *et al.* (2016)](https://doi.org/10.1016/j.jtrangeo.2015.12.008). 
This general framework relies on a two-step approach to generate mobility flows,
separating the trip distribution law, gravity or intervening opportunities, from
the modeling approach used to derive flows from this law.

## 2 Install

The `TDLM` package can be installed with the following command in an R session:

From the CRAN

``` r
install.packages("TDLM")
```

or from GitHub

``` r
# install.packages("devtools")
devtools::install_github("RTDLM/TDLM", build_vignettes = TRUE)
```

## 3 Tutorial

A tutorial vignette is available [here](https://rtdlm.github.io/TDLM/articles/TDLM.html).

## 4 Dependencies

`TDLM` depends on the following packages: `Ecume`, `mathjaxr`, `Rdpack`, `readr`, 
`rmarkdown`, and `sf`. 

Additionally, `TDLM` requires Java to function properly. Please ensure that Java 
is installed and correctly configured on your system.

## 5  Citation

Lenormand M (2023) [TDLM: An R package for a systematic comparison of
trip distribution laws and models.](https://joss.theoj.org/papers/10.21105/joss.05434#) 
*Journal of Open Source Software* 8, 5434.

Please feel free to open an issue if you encounter a problem
with the package.
