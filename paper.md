---
title: '`shar`: An `R` package to analyze species-habitat associations using point pattern analysis'
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

Studying species-habitat associations is one approach to reveal the importance of abiotic processes in shaping the spatial distribution of species.
Even though the `R` programming language offers many packages for spatial point pattern analysis, currently there is no comprehensive package specifically designed to analyze species-habitat associations.
The `shar` package builds on widely used `R` packages for spatial analyses and provides an easy and straightforward way to uncover species-habitat associations for discrete environmental data.

# Statement of need

Species-habitat associations are a result of certain species being specialized to certain environmental conditions [@Tilman1993] and typically result in a clustering of individuals at suitable habitats [@Harms2001; @Comita2007].
Thus, analyzing species-habitat associations can help to understand the importance of abiotic processes shaping the spatial distribution of species [@Garzon-Lopez2014].
However, since biotic processes (e.g., competition, limited dispersal) can also lead to a clustering of individuals, analyses of species-habitat associations need to control for potential biotic processes because they result in a violation of the independence assumption of similar statistical tests, such as the $\chi^2$ test, ordination methods, or canonical correspondence analysis [@Plotkin2000; @Harms2001].
Spatial autocorrelation of the environmental conditions could also violate the independence assumption of the previously mentioned statistical tests [@Harms2001].
For example, previous research has shown that violating the independence assumptions of the $\chi^2$ test resulted in more (i.e., possible false-positive) species-habitat associations than the more conservative methods that are provided by the `shar` package [@Plotkin2000; @Harms2001].

Ecologists use the spatial distribution of ecological objects to infer the underlying processes that shaped their distribution because spatial patterns can act as a "memory" of the processes that shaped them [@Velazquez2016].
A spatial point pattern contains the fully mapped locations (in terms of *x*,*y* coordinates) of all individual objects in a normally two-dimensional study area and assumes that the object locations can be described by discrete points [@Wiegand2014; @Velazquez2016].
For example, many studies use individual tree locations to infer the processes that determined their distribution [@Velazquez2016], and further examples include gopher mounds [@Klaas2000], gorilla nests [@Funwi-Gabga2012], "fairy circles" [@Getzin2015], or bacteria on leaves [@Esser2015a].

The `spatstat` package [@Baddeley2015] allows ecologists to access many methods of spatial point pattern analysis, such as summary functions and null model simulations, and to simulate heterogeneous point process models that can be used to show the importance of abiotic processes for continuous environmental data [@Getzin2008].
However, even though many ecological studies on species-habitat associations using discrete environmental data can be found in the literature [@John2007; @Garzon-Lopez2014; @Guo2016; @Yang2016; @Du2017; @Furniss2017], `spatstat` cannot be used to reveal such associations without larger programming efforts by the users.
The `inlabru` package provides an approach to  analyze the importance of abiotic processes, mostly for continuous environmental data, using Bayesian spatial modelling [@Bachl2019].
The `fgeo` package [@Lepore2019] allows to visualize and analyze forest diversity, including species-habitat associations.
But, the `fgeo` was designed to specifally handle ForestGEO data (<https://forestgeo.si.edu>) and furthermore includes only a subset of methods available to analyze species-habitat associations.

Thus, the `shar` package was developed to provide a comprehensive tool set to analyze species-habitat associations of spatial point patterns.
All methods in the `shar` package are designed for discrete environmental data and have the advantage of very few assumptions about the data characteristics.
In order to make the `shar` package as accessible for as many people as possible, it builds on two of the most commonly used `R` packages for spatial data, namely the `spatstat` and `raster` packages [@Hijmans2019].

# Methodological background

To analyze species-habitat associations, potential interdependence between the object locations and the environmental conditions needs to be broken by randomizing the data as a null model.
Within the field of spatial point pattern analysis, there are two related approaches to break potential dependencies [@Plotkin2000; @Harms2001].
Both require the spatial location of all objects, as well as discrete raster data for environmental conditions.

The first approach to simulate null model data is to randomize the environmental data, while keeping the object locations stable.
This can be achieved by shifting the raster data around a torus ("torus translation test") or using a random walk algorithm ("randomized-habitats procedure") [@Harms2001].
The second approach is to randomize the object locations, while keeping the environmental data stable.
This can be achieved by fitting point process models to the object locations ("gamma test") [@Plotkin2000] or using a simulated annealing approach ("pattern reconstruction") [@Kirkpatrick1983].

The two approaches differ in how they randomize the null model data, but both control for potential biotic processes by preserving the spatial structure of the data [@Plotkin2000; @Wiegand2014] and result in similar results.
Finally, species-habitat associations are present if species are found in certain habitats in the data more often than expected compared to the randomized null model data [@Plotkin2000; @Harms2001].
Given the characteristics of the method, a positive association to one habitat inevitably leads to a negative association to at least one of the other habitats (and vice versa) [@Yamada2006].

# How to use the package

Analyzing species-habitat associations is straightforward with the `shar` package.
Only two objects are needed to quantify species-habitat associations, namely a `spatstat` object that includes all object locations within the study area and a `raster` object with discrete habitat classes.

However, all methods require "fully mapped data" in the sense that NA cells of the environmental data are allowed only if simultaneously these areas cannot accommodate any locations of the point pattern (e.g., a water body within a forest area).
This needs to be reflected in the observation window of the point pattern.
For the torus translation method, no NA values are allowed at all.

To randomize the environmental data, either `translate_raster()` or `randomize_raster()` can be used.
For the former, the number of randomizations of the null model data is automatically determined by the number of rows and columns of the `raster` object.
For the later, the number of randomizations must be specified using the `n_random` argument.

```
torus_trans <- translate_raster(raster = landscape_discrete)

random_walk <- randomize_raster(raster = landscape_discrete, n_random = 39)
```

Alternatively, to randomize the object locations, either `fit_point_process()`, `reconstruct_pattern()`, or `reconstruct_pattern_marks()` can be used.
In all cases, the number of randomization must be specified using the `n_random` argument.
In order to preserve the spatial structure of the input as detailed as possible, several options are present to acknowledge for example if the input object locations are clustered or heterogeneously distributed in the study area.

```
gamma_test <- fit_point_process(pattern = species_a, n_random = 39,
                                process = "cluster")

reconstruction <- reconstruct_pattern(pattern = species_a, n_random = 39,
                                      method = "cluster")
```

Lastly, the input data and the randomized null model data are used to test if significant species-habitat associations are present.
The `results_habitat_association()` function automatically detects which part of the data was randomized and can be used identically with either of the used randomization approach.

```
results_habitat_association(pattern = species_a, raster = random_walk,
                            significance_level = 0.01)

> Input: randomized raster
> Quantile thresholds: negative < 0.005 || positive > 0.995
  habitat count lo hi significance
1       1    35 10 35         n.s.
2       2    44 19 53         n.s.
3       3    36 15 49         n.s.
4       4     4 15 58     negative
5       5    73 48 90         n.s.
```

The `shar` package also provides several utility and plotting functions such as a generic `plot()` function to plot the null model data, `calculate_energy()` to calculate the difference between the input object locations and the randomized null model data object locations, or `classify_habitats()` to classify continuous environmental data into discrete habitats. For all functions, please see the "Functions" article on the `shar` homepage (<https://r-spatialecology.github.io/shar>).

# Parallelization

One major drawback of the `shar` package is the computation time related to some of the randomization methods for null model data.
This is the case especially for pattern reconstruction, even though most point pattern analysis studies use less than 1000 null model randomizations [@Velazquez2016].
However, since the randomizations of the null model data are independent of each other, this could be parallized using available frameworks, such as the `future` [@Bengtsson2021] or
`parallel` [@RCoreTeam2021] package.
The `shar` package does not allow to run code in parallel internally to not limit users to a specific parallelization framework.
For a short example how to simulate null model data in parallel using the `future` package, please see the "Parallelization" article on the `shar` homepage (<https://r-spatialecology.github.io/shar>).
However, the presented approach could be used with any other parallelization framework as well.

# Acknowledgements

Support was provided by the German Research Association (DFG) Research Training Group 1644 "Scaling Problems in Statistics", grant number 152112243.
M.H.K.H. is thankful to Sebastian Hanss und Marco Sciaini for their help during the development of the `shar` package and Katrina Munsterman, Bridget Shayka and Samantha Iliff for comments on earlier drafts of the manuscript.
Thomas Etherington and Lionel Hertzog provided valuable feedback during the review process that improved the manuscript and the `shar` package.

# References
