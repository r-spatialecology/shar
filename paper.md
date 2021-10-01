---
title: '`shar`: A `R` package to analyze species-habitat associations using point pattern analysis'
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

Studying species-habitat associations is one tool to reveal the importance of abiotic processes in shaping the spatial distribution of species.
Even though the `R` programming language offers many packages for spatial point pattern analysis, currently there is no comprehensive package specifically designed to analyze species-habitat associations.
The `shar` package builds on widely used `R` packages for spatial analyses and provides an easy and straightforward way to uncover species-habitat associations.

# Statement of need

Species-habitat associations are a result of certain species being specialized to certain abiotic conditions [@Tilman1993] and typically result in a clustering of these species at suitable habitats [@Harms2001; @Comita2007].
Thus, analyzing species-habitat associations can help to understand the importance of abiotic processes shaping the spatial distribution of species [@Garzon-Lopez2014].
However, since biotic processes (e.g., competition, limited dispersal) can also lead to a clustering of species, analyses of species-habitat associations need to control for potential biotic processes because they result in a violation of the independence assumption of statistical tests such as the $\chi^2$ test [@Plotkin2000].

Ecologists use the spatial distribution of ecological objects to infer the underlying processes that shaped their distribution. For example, many studies use individual tree locations to infer the processes that determined their distribution [@Velazquez2016].
A spatial point pattern contains the locations of all objects in a study area and assumes that the object locations can be described by points [@Wiegand2014].
The `spatstat` package [@Baddeley2015] allows ecologists to access many methods of spatial point pattern analysis, such as summary functions and null model simulations.
However, even though many ecological studies on species-habitat associations can be found in the literature [@John2007; @Garzon-Lopez2014; @Guo2016; @Yang2016; @Du2017; @Furniss2017], `spatstat` cannot be used to reveal species-habitat associations without larger programming efforts by the users.

Thus, the `shar` package was developed to provide a tool set to analyze species-habitat associations of spatial point patterns.
In order to make the package as accessible for as many people as possible, it builds on two of the most commonly used `R` packages for spatial data, namely the `spatstat` and `raster` packages [@Hijmans2019].

# Methodological background

To analyze species-habitat associations, potential dependencies between the object locations and the environmental conditions need to be broken by randomizing the data as null model.
Within the field of spatial point pattern analysis, there are two related approaches to break potential dependencies [@Plotkin2000; @Harms2001].
Both require the spatial location of all objects, as well as raster data for environmental conditions.

The first approach to simulate null model data, is to randomizes the environmental data, while keeping the object locations stable.
This can be achieved by shifting the raster data around a torus ("Torus translation test") or using a random walk algorithm ("Randomized-habitats procedure") [@Harms2001].
The second approach, is to randomizes the object locations, while keeping the environmental data stable.
This can be achieved by fitting point process models to the object locations ("gamma test") [@Plotkin2000] or using a simulated annealing approach ("pattern reconstruction") [@Kirkpatrick1983].

The two approaches differ in how they randomize the null model data, but both control for potential biotic processes by preserving the spatial structure of the data [@Plotkin2000; @Wiegand2014] and result in similar results.
Finally, species-habitat associations are present if species are found in certain habitats in the data  more often than expected compared to the randomized null model data [@Plotkin2000; @Harms2001].

# How to use the package

Analyzing species-habitat associations is straightforward with the `shar` package.
Only two objects are needed to quantify species-habitat associations, namely a `spatstat` object that includes all object locations within the study area and a `raster` object with discrete habitat classes and

To randomize the environmental data, either `translate_raster()` or `randomize_raster` can be used.
For the former, the number of randomizations of the null model data is automatically determined by the number of rows and columns of the `raster` object.
For the later, the number of randomizations must be specified using the `r_random` argument.

```
torus_trans <- translate_raster(raster = landscape_discrete)

random_walk <- randomize_raster(raster = landscape_discrete, n_random = 39)
```

Alternatively, to randomize the object locations, either `fit_point_process` or `reconstruct_pattern_*` can be used.
In both cases, the number of randomization must be specified using the `r_random` argument.
In order to preserve the spatial structure of the input as detailed as possible, several options are present to acknowledge for example if the input object locations are clustered or heterogeneously distributed in the study area.

```
gamma_test <- fit_point_process(pattern = species_a, n_random = 39,
                                process = "cluster")

reconstruction <- reconstruct_pattern_homo(pattern = species_a, n_random = 39)
```

Lastly, the input data and the randomized null model data are used to test if significant species-habitat associations are present.
The `results_habitat_association()` function automatically detects which part of the data was randomized and can be used identically with either of the used randomization approach.

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
M.H.K.H. is thankful to Sebastian Hanss und Marco Sciaini for their help during the development of the `shar` package and Katrina Munsterman, Bridget Shayka and Samantha Iliff for comments on earlier drafts of the manuscript.

# References
