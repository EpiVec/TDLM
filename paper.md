---
title: 'TDLM: An R package for a systematic comparison of trip distribution laws and models'
tags:
- R
- Spatial Interaction Models
- Spatial networks
- Commuting networks
- Gravity model
- Radiation model
date: "17 March 2023"
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
interactions between locations. These models are usually based on the
characteristics of the locations and the way they are spatially distributed. 
Interactions between locations can take several forms, population movements, 
widely studied in geography, transportation research and urban planning is one 
of them. The flows of individuals between locations is usually represented by a
trip table better known as Origin-Destination (OD) matrix 
[@Lenormand2016;@Barbosa2018]. The estimation of OD matrices is part of the 
four-step travel model in transportation research. It corresponds to the second
step, called trip distribution, the aim of which is to match the trip origins 
with the trip destinations using a spatial interaction model commonly referred 
to as trip distribution model in the four-step travel framework. 

In order to facilitate the use and comparison of trip distribution models, and 
more generally spatial distribution models, we present **TDML** an R package 
proposing a set of easy-to-use functions to rigorously and fairly compare 
trip distribution laws and models as described in @Lenormand2016.

# Statement of need

Trip distribution model are generally composed of two mechanisms, one mechanism
based on a 'law' to estimate the probability that an individual move from one 
location to another and a second mechanism based on a 'model' used to estimate 
the number of individuals moving from one location to another. These two 
mechanisms are rarely dissociated which could lead to methodological flaws when 
comparing different laws and/or models [@Lenormand2012;@Simini2012;@Masucci2013;
@Yang2014].

To overcome these limitations, the **TDML** R package is based on a two-step 
approach to generate mobility flows by separating the trip distribution law, 
gravity or intervening opportunities, from the modeling approach used to 
generate the flows from this law.

# References
