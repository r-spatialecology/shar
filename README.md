
[![Travis build
status](https://travis-ci.org/mhesselbarth/SHAR.svg?branch=master)](https://travis-ci.org/mhesselbarth/SHAR)
[![Coverage
status](https://codecov.io/gh/mhesselbarth/SHAR/branch/master/graph/badge.svg)](https://codecov.io/github/mhesselbarth/SHAR?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/SHAR)](https://cran.r-project.org/package=SHAR)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# SHAR

SHAR is a R package for analyzing species habitat associations.
Therefore, information about the location of the species is needed (as a
point pattern) and about the environmental conditions (as a raster map).
In order to analyse the data for significant habitat associations either
the location data or the environmental data is randomized n-times. Then,
counts within the habitats are compared between the randomized data and
the observed data. Positive or negative associations are present if the
observed counts is higher or lower than the randomized counts (using
quantile thresholds). Methods are mainly described in Plotkin et al.
(2000), Harms et al. (2001) and Wiegand & Moloney (2014). SHAR is mainly
based on the `spatstat` (Baddeley et al. 2015) and `raster` (Hijmans
2017) package.

## Installation

You can install the released version of SHAR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SHAR") # Nope, not yet...
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mhesselbarth/SHAR")
```

## How to use SHAR

SHAR comes with build-in example data sets. `landscape` contains
examplary continious environmental data. However, all methods depend on
discrete data. Therefore we need to classify the data first.

``` r
library(SHAR)
library(raster)

landscape_classified <- classify_habitats(raster = landscape, classes = 5)
```

`species_a` and `species_b` are examplary location of species,
e.g. trees, as `ppp`-objects from the `spatstat` package.

There are two possibilities to randomize the environmental data, both
described in Harms et al. (2001). The first shifts the habitat map in
all 4 cardinal directions around a torus. The second one assigns the
habitat values to an empty map using a random walk algorithm.

``` r
torus_trans <- translate_raster(raster = landscape_classified)

random_walk <- randomize_raster(raster = landscape_classified, n_random = 39) # takes some time
```

To randomize the point pattern, either use the Gamma test described by
Plotkin et al. (2000) or pattern reconstruction (Tscheschel & Stoyan
2006)
.

``` r
gamma_test <- fit_point_process(pattern = species_a, process = "poisson", n_random = 39)

reconstruct <- reconstruct_pattern(pattern = species_b, max_runs = 2500, n_random = 39) # takes some time
```

The data was created that `species_a` has a negative association to
habitat 4 and `species_b` has a positive association to habitat 5. At
one point a posititive association to one habitat leads consequently to
a negative association to another habitat (and vice versa). All this can
be seen in the results.

``` r
results_habitat_association(pattern = species_a, raster = torus_trans)
#> > Input: randomized raster | Thresholds: negative < 0.025 - positive > 0.975
#>   habitat count lo hi significance
#> 1       1    10  0  8     positive
#> 2       2    14  8 24         n.s.
#> 3       3    30 14 29     positive
#> 4       4     0 10 26     negative
#> 5       5    14  4 17         n.s.
results_habitat_association(pattern = species_b, raster = random_walk)
#> > Input: randomized raster | Thresholds: negative < 0.025 - positive > 0.975
#>   habitat count    lo    hi significance
#> 1       1     7  3.00 23.05         n.s.
#> 2       2    24 26.95 63.10     negative
#> 3       3    36 46.95 90.05     negative
#> 4       4    19 28.90 76.40     negative
#> 5       5   114 11.75 47.30     positive

results_habitat_association(pattern = gamma_test, raster = landscape_classified)
#> > Input: randomized point pattern | Thresholds: negative < 0.025 - positive > 0.975
#>   habitat count    lo    hi significance
#> 1       1    10  0.00  6.10     positive
#> 2       2    14  8.85 21.05         n.s.
#> 3       3    30 16.90 27.25     positive
#> 4       4     0 11.85 24.10     negative
#> 5       5    14  4.00 15.05         n.s.
results_habitat_association(pattern = reconstruct, raster = landscape_classified)
#> > Input: randomized point pattern | Thresholds: negative < 0.025 - positive > 0.975
#>   habitat count    lo    hi significance
#> 1       1     7  1.00 21.25         n.s.
#> 2       2    24 25.85 71.30     negative
#> 3       3    36 44.90 86.10     negative
#> 4       4    19 31.70 64.20     negative
#> 5       5   114 11.95 58.35     positive
```

Of coures, there are several utility functions. For example, you can
plot a randomized pattern or calculate the differences between the
observed pattern and the randomized patterns (using summary functions).

``` r
plot_randomized_pattern(reconstruct)
```

<img src="man/figures/README-plot random pattern-1.png" width="100%" />

``` r

calculate_energy(reconstruct)
#>  randomized_1  randomized_2  randomized_3  randomized_4  randomized_5 
#>    0.12383933    0.11885764    0.06221139    0.10145441    0.08672217 
#>  randomized_6  randomized_7  randomized_8  randomized_9 randomized_10 
#>    0.07031200    0.08997604    0.07393047    0.10279469    0.10767068 
#> randomized_11 randomized_12 randomized_13 randomized_14 randomized_15 
#>    0.10526385    0.09278873    0.08716073    0.07667916    0.10519324 
#> randomized_16 randomized_17 randomized_18 randomized_19 randomized_20 
#>    0.12727424    0.09034474    0.08869269    0.10267411    0.06574193 
#> randomized_21 randomized_22 randomized_23 randomized_24 randomized_25 
#>    0.07093437    0.07157113    0.08903972    0.11520388    0.08030586 
#> randomized_26 randomized_27 randomized_28 randomized_29 randomized_30 
#>    0.08439971    0.07392913    0.07722876    0.09209930    0.08618923 
#> randomized_31 randomized_32 randomized_33 randomized_34 randomized_35 
#>    0.08711860    0.10440259    0.08059639    0.09564745    0.06967828 
#> randomized_36 randomized_37 randomized_38 randomized_39 
#>    0.10530861    0.08109449    0.12068153    0.09089023
```

## References

Baddeley, A., Rubak, E., Turner, R. (2015). Spatial Point Patterns:
Methodology and Applications with R. London:Chapman and Hall/CRC Press,
2015.
<http://www.crcpress.com/Spatial-Point-Patterns-Methodology-and-Applications-with-R/Baddeley-Rubak-Turner/9781482210200/>

Harms, K. E., Condit, R., Hubbell, S. P., & Foster, R. B. (2001).
Habitat associations of trees and shrubs in a 50-ha neotropical forest
plot. Journal of Ecology, 89(6), 947–959.

Hijmans, R. J. (2017). raster: Geographic Data Analysis and Modeling. R
package version 2.6-7. <https://CRAN.R-project.org/package=raster>

Plotkin, J. B., Potts, M. D., Leslie, N., Manokaran, N., LaFrankie, J.
V., & Ashton, P. S. (2000). Species-area curves, spatial aggregation,
and habitat specialization in tropical forests. Journal of Theoretical
Biology, 207(1), 81–99.

Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of
random point patterns. Computational Statistics and Data Analysis,
51(2), 859–871.

Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern
analysis in ecology. Boca Raton: Chapman and Hall/CRC Press.
