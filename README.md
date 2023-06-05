# __TDLM__ <img src="man/figures/logo.png" align="right" alt="" width="200" />

# Systematic comparison of trip distribution laws and models 

<!-- badges: start -->
[![R-CMD-check](https://github.com/EpiVec/TDLM/workflows/R-CMD-check/badge.svg)](https://github.com/EpiVec/TDLM/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/TDLM)](https://cran.r-project.org/package=TDLM)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/TDLM)](http://r-pkg.org/pkg/TDLM)
[![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg)](https://www.r-project.org/Licenses/GPL-3) 
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

Lenormand M, Bassolas A & Ramasco JJ (2016) [Systematic comparison of trip distribution laws and models.](https://www.sciencedirect.com/science/article/abs/pii/S0966692315002422) *Journal of Transport Geography* 51, 158-169.

## 6 Contribution instructions

Please feel free to open an issue if you encounter a problem
with the package.

You can also reach me at maxime.lenormand[at]inrae.fr.

