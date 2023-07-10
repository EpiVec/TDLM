---
title: 'TDLM: An R package for a systematic comparison of trip distribution laws and models'
tags:
- R
- Spatial Interaction Models
- Spatial networks
- Commuting networks
- Gravity model
- Radiation model
date: "5 July 2023"
output: pdf_document
authors:
- name: "Maxime Lenormand"
  orcid: "0000-0001-6362-3473"
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: TETIS, Univ Montpellier, AgroParisTech, CIRAD, CNRS, INRAE, Montpellier, France
  index: 1
---

# Summary

Spatial interaction models are widely used to estimate and explain spatial
interactions between geographical areas or locations. These models are usually 
based on the characteristics of the locations and the way they are 
spatially distributed. Interactions between locations can take several forms, 
population movements, widely studied in geography, transportation research and 
urban planning is one of them. The flows of individuals between locations is 
usually represented by a trip table better known as Origin-Destination (OD) 
matrix [@Lenormand2016;@Barbosa2018]. The estimation of OD matrices is part of 
the four-step travel model in transportation research. It corresponds to the 
second step, called trip distribution, the aim of which is to match the trip 
origins with the trip destinations using a spatial interaction model 
commonly referred to as trip distribution model in the four-step travel 
framework. 

In order to facilitate the use and comparison of trip distribution models, and 
more generally spatial distribution models, we present **TDML** an R package 
proposing a set of easy-to-use functions to rigorously and fairly compare 
trip distribution laws and models as described in @Lenormand2016.

# Statement of need

Trip distribution models are generally composed of two mechanisms, one mechanism
based on a 'law' to estimate the probability that an individual move from one 
location to another and a second mechanism based on a 'model' used to estimate 
the number of individuals moving from one location to another. These two 
mechanisms are rarely dissociated which could lead to methodological flaws when 
comparing different laws and/or models [@Lenormand2012;@Simini2012;@Masucci2013;
@Yang2014]. This is particularly important when we compare the two historical
approaches - gravity and intervening opportunities - for the estimation of 
trip distribution. 

We identified several R packages providing an implementation of spatial 
interaction models that can be used to estimate trip distributions. 
The **gravity** package [@Wolwer2018], the **spflow** 
package [@Dargel2021], the **mobility** [@Giles2021] and the **simodels** 
package [@Lovelace2023]. The **gravity**, **spflow** and **mobility** packages 
are based on statistical models and have not been designed to compare gravity 
and intervening opportunities laws and constrained models independently. 
Although the package structure and functionality of **simodels** are very 
different from that of **TDLM**, it offers the possibility to compare trip 
distribution laws and models independently. **simodels** proposes 
an interesting approach by not defining (nor encouraging the use of) any 
particular trip distribution laws, but this also makes the systematic comparison 
of gravity and intervening opportunities laws more complicated for 
non-expert users. Furthermore, **simodels** does not offer the possibility at 
this stage to use the doubly constrained model or any functionality to 
systematically compare observed and simulated OD matrices.
 
To overcome these limitations, the **TDML** R package is based on a two-step 
approach to generate mobility flows by separating the trip distribution law, 
gravity or intervening opportunities, from the modeling approach used to 
generate the flows from this law. 

# Functionality

**TDLM** is available on [CRAN](https://cran.r-project.org/package=TDLM) and 
[GitHub](https://github.com/EpiVec/TDLM/). The **TDLM**'s website includes a [tutorial](https://epivec.github.io/TDLM/articles/TDLM.html) 
describing the functions of this package with an illustrative example based on 
commuting data from US Kansas in 2000.

**TDLM** features four main functions for generating OD matrices
based on a wide range of trip distribution laws and models and for evaluating
the simulated matrices against observed data. 

* **run_law_model** is the main function of the package. This function estimates
mobility flows using different distribution laws (four gravity laws, three 
intervening opportunity laws and a uniform law) and models (unconstrained, 
production constrained, attraction constrained and doubly constrained). The 
function has two sets of arguments, one for the law and another one for the 
model. 

* **run_law** estimates mobility flows using different distribution laws. It 
is based on the first step of the two-step proposed approach to generate a 
probability distribution based on the different laws.

* **run_model** estimates mobility flows using different distribution models. It
based on the second step of the two-step proposed approach to generate mobility 
flow based on a matrix of probabilities using different constrained models.

* **gof** computes goodness-of-fit measures between observed and simulated 
OD matrices. Six goodness-of-fit measures have been considered at this stage.

**TDLM** includes utility functions to check, format and generate the inputs 
data and help to calibrate the trip distribution laws. 

# References
