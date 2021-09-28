---
title: 'shar: A R package to analyse species-habitat associations using point pattern analysis'
tags:
  - R
  - open source software
  - spatial point pattern analysis
  - gamma-test
  - pattern reconstruction
  - torus-translation test
  - randomized-habitats procedure;
authors:
  - name: Maximilian H.K. Hesselbarth^[corresponding author]
    orcid: 0000-0003-1125-9918
    affiliation: "1, 2"
affiliations:
 - name: Department of Ecosystem Modelling, University of Goettingen, Buesgenweg 4, 37077, Goettingen
   index: 1
 - name: Department of Ecology and Evolutionary Biology, University of Michigan, 1105 N University Ave, Ann Arbor, Michigan 48109, USA
   index: 2
date: 2021-09-28
bibliography: paper.bib
---

# Summary

Lorem ipsum dolor sit amet, consectetuer adipiscing elit Aenean commodo ligula eget dolor.
Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus, donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem.
Nulla consequat massa quis enim Donec pede justo, fringilla vel, aliquet nec, vulputate.

# Statement of need

Species-habitat associations are a result of certain species being specialized to certain environmental (also referred to as abiotic) conditions [@Tilman1993] and typically result in a clustering of these species at suitable habitats [@Harms2001; @Comita2007].
Thus, analyzing species-habitat associations can help to understand the influence and importance of abiotic processes shaping the spatial species distribution [@Garzon-Lopez2014].
However, since also biotic processes (e.g., competition, limited seed dispersal) can lead to a clustering of species, a compelling analysis needs to control for potential biotic processes.

A spatial point pattern consists the locations of all ecological objects in a study area and assumes that the object locations can be described by points [@Wiegand2014].
Ecologists use spatial point patterns to conclude the underlying ecological processes that shaped the patterns and many studies using for example individual tree locations can be found in the literature [@Velazquez2016].
The comprehensive and excellent `spatstat` package [@Baddeley2015] allows ecologists to access many methods of spatial point pattern analysis, such as summary functions and many common null model simulations.
However, even though many ecological studies analyzing species-habitat associations, using mostly temperate and tropical forest tree point patterns, can be found in the literature [e.g., @John2007; @Garzon-Lopez2014; @Guo2016; @Yang2016; @Du2017; @Furniss2017], `spatstat` does not allow to straightforward analyze species-habitat associations.

Thus, the `shar` package was developed to provide an "out-of-the-box" tool set to analyze species-habitat associations of spatial point patterns.
In order to make the package as accessible as possible for as many people as possible, it is based on two of the most common used `R` packages to analyze spatial data, namely the `spatstat` and `raster` packages [@Hijmans2019].

# Methodological background

Within the field of spatial point pattern analysis, there are two contrasting approaches to study species-habitat associations [@Plotkin2000; @Harms2001].
Both have in common that additionally to the spatial location of all ecological objects, also raster data about the environmental conditions is required.

# How to use the package

# Acknowledgements

M.H.K.H. was supported by the German Research Association (DFG) Research Training Group 1644 "Scaling Problems in Statistics", grant number 152112243.
M.H.K.H. is thankful to Sebastian Hanss und Marco Sciaini for their help and contribution during the development of the shar package.

# References
