
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/r-spatialecology/shar.svg?branch=master)](https://travis-ci.org/r-spatialecology/shar)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/08hgwkr82pqb6ykq/branch/master?svg=true)](https://ci.appveyor.com/project/mhesselbarth/shar/branch/master)
[![Coverage
status](https://codecov.io/gh/r-spatialecology/shar/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatialecology/shar?branch=master)
[![Project Status: Active â€“ The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/shar)](https://cran.r-project.org/package=shar)
[![](http://cranlogs.r-pkg.org/badges/grand-total/shar)](http://cran.rstudio.com/web/packages/shar/index.html)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# shar

**Sp**ecies-**H**abitat **A**ssociations in **R** is a `R` package to
analyze species-habitat associations. Therefore, information about the
location of the species is needed (as a point pattern) and about the
environmental conditions (as a raster map). In order to analyse the data
for significant habitat associations either the location data or the
environmental data is randomized n-times. Then, counts within the
habitats are compared between the randomized data and the observed data.
Positive or negative associations are present if the observed counts is
higher or lower than the randomized counts (using quantile thresholds).
Methods are mainly described in Plotkin et al. (2000), Harms et al.
(2001) and Wiegand & Moloney (2014). **shar** is mainly based on the
`spatstat` (Baddeley et al. 2015) and `raster` (Hijmans 2017) package.

## Installation

You can install the released version of shar from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("shar") # Nope, not yet...
```

And the development version from
[GitHub](https://github.com/r-spatialecology/shar) with:

``` r
# install.packages("devtools")
devtools::install_github("r-spatialecology/shar")
```

## How to use shar

``` r
library(shar)
library(raster)
```

shar comes with build-in example data sets. `species_a` and `species_b`
are examplary location of species, e.g. trees, as `ppp`-objects from the
`spatstat` package. `landscape` contains examplary continious
environmental data. However, all methods depend on discrete data.
Therefore we need to classify the data
first.

``` r
landscape_classified <- classify_habitats(raster = landscape, classes = 5)
```

There are two possibilities to randomize the environmental data, both
described in Harms et al. (2001). The first shifts the habitat map in
all 4 cardinal directions around a torus. The second one assigns the
habitat values to an empty map using a random walk algorithm. Both
functions return a list with randomized rasters and the observed
one.

``` r
torus_trans <- translate_raster(raster = landscape_classified, verbose = FALSE)

random_walk <- randomize_raster(raster = landscape_classified, n_random = 19, verbose = FALSE)
```

<img src="man/figures/README-plot_habitat_random-1.png" width="100%" />

To randomize the point pattern, either use the Gamma test described by
Plotkin et al. (2000) or pattern reconstruction (Tscheschel & Stoyan
2006).

``` r
gamma_test <- fit_point_process(pattern = species_a, process = "cluster", n_random = 19, verbose = FALSE)

reconstruct <- reconstruct_pattern(pattern = species_b, max_runs = 500, n_random = 19, verbose = FALSE) # takes some time
```

Of coures, there are several utility functions. For example, you can
plot a randomized pattern or calculate the differences between the
observed pattern and the randomized patterns (using summary functions).

``` r
plot_randomized_pattern(reconstruct)
```

<img src="man/figures/README-plot_random_pattern-1.png" width="100%" />

``` r

calculate_energy(reconstruct)
#>  randomized_1  randomized_2  randomized_3  randomized_4  randomized_5 
#>    0.06030756    0.05936283    0.05819350    0.06859139    0.06856632 
#>  randomized_6  randomized_7  randomized_8  randomized_9 randomized_10 
#>    0.07287071    0.06409940    0.06361426    0.06129393    0.04715134 
#> randomized_11 randomized_12 randomized_13 randomized_14 randomized_15 
#>    0.05203248    0.05572710    0.05258205    0.07844003    0.05031089 
#> randomized_16 randomized_17 randomized_18 randomized_19 
#>    0.06423693    0.06101793    0.07234846    0.07822852
```

The data was created that `species_a` has a negative association to
habitat 4 and `species_b` has a positive association to habitat 5. At
one point a posititive association to one habitat leads consequently to
a negative association to another habitat (and vice versa). All this can
be seen in the results.

``` r
results_habitat_association(pattern = species_a, raster = torus_trans)
#> > Input: randomized raster | Quantile thresholds: negative < 0.025 - positive > 0.975
#>   habitat count lo hi significance
#> 1       1    10  0  8     positive
#> 2       2    14  8 24         n.s.
#> 3       3    30 14 29     positive
#> 4       4     0 10 26     negative
#> 5       5    14  4 17         n.s.

results_habitat_association(pattern = reconstruct, raster = landscape_classified)
#> > Input: randomized point pattern | Quantile thresholds: negative < 0.025 - positive > 0.975
#>   habitat count    lo    hi significance
#> 1       1     7  3.00 22.10         n.s.
#> 2       2    20 32.00 66.75     negative
#> 3       3    31 47.45 79.55     negative
#> 4       4    33 35.35 63.55     negative
#> 5       5   109 12.35 44.10     positive
```

## References

Baddeley, A., Rubak, E., Turner, R. (2015). Spatial Point Patterns:
Methodology and Applications with R. London:Chapman and Hall/CRC Press,
2015.
<http://www.crcpress.com/Spatial-Point-Patterns-Methodology-and-Applications-with-R/Baddeley-Rubak-Turner/9781482210200/>

Harms, K. E., Condit, R., Hubbell, S. P., & Foster, R. B. (2001).
Habitat associations of trees and shrubs in a 50-ha neotropical forest
plot. Journal of Ecology, 89(6), 947-959.

Hijmans, R. J. (2017). raster: Geographic Data Analysis and Modeling. R
package version 2.6-7. <https://CRAN.R-project.org/package=raster>

Plotkin, J. B., Potts, M. D., Leslie, N., Manokaran, N., LaFrankie, J.
V., & Ashton, P. S. (2000). Species-area curves, spatial aggregation,
and habitat specialization in tropical forests. Journal of Theoretical
Biology, 207(1), 81-99.

Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of
random point patterns. Computational Statistics and Data Analysis,
51(2), 859-871.

Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern
analysis in ecology. Boca Raton: Chapman and Hall/CRC Press.
