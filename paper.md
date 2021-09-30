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

Analysing species-habitat associations is one tool to study the importance of abiotic processes in shaping the spatial distribution ecological objects.
Even though the `R` programming language offers many packages for spatial point pattern analysis in general, currently there is no comprehensive package specifically designed to analyse species-habitat associations.
The `shar` package is build on widely used `R` packages for spatial analyses and provides an easy and straightforward way to analyse species-habitat associations.

# Statement of need

Species-habitat associations are a result of certain species being specialised to certain abiotic conditions [@Tilman1993] and typically result in a clustering of these species at suitable habitats [@Harms2001; @Comita2007].
Thus, analysing species-habitat associations can help to understand the importance of abiotic processes shaping the spatial species distribution [@Garzon-Lopez2014].
However, since also biotic processes (e.g., competition, limited dispersal) can lead to a clustering of species, analyses of species-habitat associations need to control for potential biotic processes because they result in a violation of the independence assumption of alternatives such as the $\chi^2$ test [@Plotkin2000].

A spatial point pattern consists the locations of all objects in a study area and assumes that the object locations can be described by points [@Wiegand2014].
Ecologists use spatial point patterns to conclude the underlying ecological processes that shaped the patterns and many studies using for example individual tree locations can be found in the literature [@Velazquez2016].
The `spatstat` package [@Baddeley2015] allows ecologists to access many methods of spatial point pattern analysis, such as summary functions and null model simulations.
However, even though many ecological studies analysing species-habitat associations can be found in the literature [e.g., @John2007; @Garzon-Lopez2014; @Guo2016; @Yang2016; @Du2017; @Furniss2017], `spatstat` does not allow for straightforward analysis of species-habitat associations without bigger programming efforts by the users.

Thus, the `shar` package was developed to provide an "out-of-the-box" tool set to analyse species-habitat associations of spatial point patterns.
In order to make the package as accessible for as many people as possible, it builds on two of the most common used `R` packages to analyse spatial data, namely the `spatstat` and `raster` packages [@Hijmans2019].

# Methodological background

Within the field of spatial point pattern analysis, there are two related approaches to study species-habitat associations [@Plotkin2000; @Harms2001].
Both require the spatial location of all objects, as well as raster data for environmental conditions.
To analyse species-habitat associations, potential dependencies between the object locations and the environmental conditions need to be broken by randomizing the data as null model data.
However, to control for potential biotic processes, the spatial structure of the data must be preserved [@Plotkin2000; @Wiegand2014].
The two approaches differ in how they randomize the null model data, while preserving the spatial structure, but nevertheless result in similar results.
Finally, species-habitat associations are present if species can be found more often in certain habitats in the data than expected compared to the randomized null model data [@Plotkin2000; @Harms2001].

The first approach to simulate null model data randomizes the environmental data, while keeping the object locations stable.
This can be achieved by shifting the raster data around a torus ("Torus translation test") or using a random walk algorithm ("Randomized-habitats procedure") [@Harms2001].

The second approach to simulate null model data randomizes the object locations, while keeping the environmental data stable.
This can be achieved by fitting point process models to the object locations ("gamma test") [@Plotkin2000] or using a simulated annealing approach ("pattern reconstruction") [@Kirkpatrick1983].

# How to use the package

Analysing species-habitat associations is straightforward with the `shar` package.
Only two objects are needed to analyse species-habitat associations, namely a `raster` object with discrete habitat classes and a `spatstat` object with all object locations within the study area.

To randomize the environmental data either `translate_raster()` or `randomize_raster` can be used.
For the former, the number of randomizations of the null model data is automatically determined by the number of rows and columns of the `raster` object.
For the later, the number of randomizations must be specified using the `r_random` argument.

```
torus_trans <- translate_raster(raster = landscape_discrete)

random_walk <- randomize_raster(raster = landscape_discrete, n_random = 39)
```

Alternatively, to randomize the object locations either `fit_point_process` or `reconstruct_pattern_*` can be used.
In both cases, the number of randomization must be specified using the `r_random` argument.
In order to preserve the spatial structure of the input as detailed as possible, several options are present to acknowledge for example if the input object locations are clustered are heterogeneously distributed in the study area.

```
gamma_test <- fit_point_process(pattern = species_a, n_random = 39,
                                process = "cluster")

reconstruction <- reconstruct_pattern_homo(pattern = species_a, n_random = 39)
```

Lastly, the input data and the randomized null model data is used to analyse if species-habitat associations are present.
The `results_habitat_association()` function automatically detects which part of the data was randomized and can be used identically regardless of the used randomization approach.

```
results_habitat_association(pattern = species_a, raster = random_walk)

> Input: randomized raster
> Quantile thresholds: negative < 0.025 || positive > 0.975
habitat   count   lo    hi    significance
1         9       2.95  13.15 n.s.
2         25      8.95  25.30 n.s.
3         27      11.00 23.20 positive
4         0       13.95 27.30 negative
5         12      6.00  17.05 n.s.
```

The `shar` packages also provides several utility and plotting functions such as `plot_randomized_raster()` and `plot_randomized_pattern()` to plot the null model data, `calculate_energy()` to calculate the difference between the input object locations and the randomized null model data object locations, or `classify_habitats()` to classify continuous environmental data into discrete habitats.  

# Acknowledgements

Supported was provided by the German Research Association (DFG) Research Training Group 1644 "Scaling Problems in Statistics", grant number 152112243.
M.H.K.H. is thankful to Sebastian Hanss und Marco Sciaini for their help during the development of the `shar` package and Katrina Munsterman and Samantha Iliff for comments on earlier drafts of the manuscript.

# References
