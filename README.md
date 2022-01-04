
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **shar** | **S**pecies **h**abitat **a**ssociations in **R** <img src="man/figures/logo.png" align="right" alt="" width="150" />

<!-- badges: start -->

| CI                                                                                                                                                                                   | Development                                                                                                                        | CRAN                                                                                                                    | License                                                                                                                                              |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| [![R-CMD-check](https://github.com/r-spatialecology/shar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-spatialecology/shar/actions/workflows/R-CMD-check.yaml) | [![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)                       | [![CRAN status](https://www.r-pkg.org/badges/version/shar)](https://cran.r-project.org/package=shar)                    | [![JOSS](https://joss.theoj.org/papers/1b786c028a5425858cb0e5428bd9173b/status.svg)](https://joss.theoj.org/papers/1b786c028a5425858cb0e5428bd9173b) |
| [![codecov](https://codecov.io/gh/r-spatialecology/shar/branch/main/graph/badge.svg?token=XMo844ABs4)](https://codecov.io/gh/r-spatialecology/shar)                                  | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) | [![CRAN logs](http://cranlogs.r-pkg.org/badges/grand-total/shar)](http://cran.rstudio.com/web/packages/shar/index.html) | [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)                                      |

<!-- badges: end -->

**S**pecies-**h**abitat **a**ssociations in **R** is a `R` package to
analyze species-habitat associations. Therefore, information about the
location of the species is needed (as a point pattern) and about the
environmental conditions (as a raster map). In order to analyse the data
for significant habitat associations either the location data or the
environmental data is randomized n-times. Then, counts within the
habitats are compared between the randomized data and the observed data.
Positive or negative associations are present if the observed counts is
higher or lower than the randomized counts (using quantile thresholds).
Methods are mainly described in Plotkin et al. (2000), Harms et
al. (2001) and Wiegand & Moloney (2014). **shar** is mainly based on
the [`spatstat`](http://spatstat.org) (Baddeley et al. 2015) and
[`raster`](https://rspatial.org/raster/) (Hijmans 2017) package.

#### Citation

The **shar** package is part of our academic work. To cite the package
or acknowledge its use in publications, please cite the following paper.

> Hesselbarth, M.H.K., (2021). shar: A R package to analyze
> species-habitat associations using point pattern analysis. Journal of
> Open Source Software, 6(67), 3811.
> <https://doi.org/10.21105/joss.03811>

The get a BibTex entry, please use `citation("shar")`.

## Installation

You can install the released version of **shar** from
[CRAN](https://cran.r-project.org/web/packages/shar/index.html) with:

``` r
install.packages("shar")
```

And the development version from
[GitHub](https://github.com/r-spatialecology/shar) with:

``` r
install.packages("remotes")

remotes::install_github("r-spatialecology/shar")
```

This also automatically installs all non-base `R` package dependencies,
namely the following packages: `classInt`, `raster`, `spatstat.core`,
`spatstat.geom`.

## How to use shar

``` r
library(shar)
library(raster)

set.seed(42)
```

**shar** comes with build-in example data sets. `species_a` and
`species_b` are exemplary location of species, e.g. trees, as
`ppp`-objects from the `spatstat` package. `landscape` contains
exemplary continuous environmental data. However, all methods depend on
discrete data. Therefore we need to classify the data first. However,
all methods require “fully mapped data” in a sense that NA cells of the
environmental data are allowed only if simultaneously these areas cannot
accommodate any locations of the point pattern (e.g., a water body
within a forest area). This needs to be reflected in the observation
window of the point pattern. For the torus translation method, no NA
values are allowed at all.

``` r
landscape_classified <- classify_habitats(raster = landscape, classes = 5)
```

There are two possibilities to randomize the environmental data, both
described in Harms et al. (2001). The first shifts the habitat map in
all 4 cardinal directions around a torus. The second one assigns the
habitat values to an empty map using a random walk algorithm. Both
functions return a list with randomized rasters and the observed one.
For more information on the methods, please click
[here](https://r-spatialecology.github.io/shar/articles/articles/background.html).

``` r
torus_trans <- translate_raster(raster = landscape_classified)

random_walk <- randomize_raster(raster = landscape_classified, n_random = 99)
```

To plot the randomized raster, you can use the plot function and specify
the number of raster as as well as the color palette used for the
discrete environmental data.

``` r
col_palette <- c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")

plot(torus_trans, n = 3, col = col_palette)
```

To randomize the point pattern, either use the Gamma test described by
Plotkin et al. (2000) or pattern reconstruction (Kirkpatrick et
al. 1983; Tscheschel & Stoyan 2006).

``` r
gamma_test <- fit_point_process(pattern = species_b, process = "cluster", n_random = 99)

# (this can takes some time)
reconstruction <- reconstruct_pattern(pattern = species_b, n_random = 99, e_threshold = 0.05)
```

Of course, there are several utility functions. For example, you can
plot the summary function of the observed pattern and the simulation
envelopes of randomized patterns (`what = "sf"`) or some randomized and
the observed pattern (`what = "pp"`) using the plot function.

``` r
plot(reconstruction, what = "pp")
```

<img src="man/figures/README-plot-random_pattern-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

Another utility functions allows to calculate the differences between
the observed pattern and the randomized patterns (also called energy
using summary functions).

``` r
calculate_energy(reconstruction, return_mean = TRUE)
## [1] 0.04908566
```

The data was created that `species_a` has a negative association to
habitat 4 and `species_b` has a positive association to habitat 5, which
is reflected in the results.

Given the characteristics of the method, a positive association to one
habitat inevitably leads to a negative association to at least one of
the other habitats (and vice versa; Yamada et al. 2006). For example, a
high amount of individual points in the positively associated habitat
simultaneously mean that less individual points can be present in the
other habitats.

Furthermore, please be aware that due to the randomization of the null
model data, results might slightly differ between different
randomization approaches (e.g., `fit_point_process()`
vs. `translate_raster()`) and even for repetitions of the same
approach. Thus, the exact `lo` and `hi` thresholds might be slightly
different when re-running the examples. However, the counts of the
observed data should be identical, and general results and trends should
be similar.

``` r
significance_level <- 0.01

results_habitat_association(pattern = species_a, raster = torus_trans, significance_level = significance_level)
## > Input: randomized raster
## > Quantile thresholds: negative < 0.005 || positive > 0.995
##   habitat count lo hi significance
## 1       1    35 10 35         n.s.
## 2       2    44 19 53         n.s.
## 3       3    36 15 49         n.s.
## 4       4     4 15 58     negative
## 5       5    73 48 90         n.s.

results_habitat_association(pattern = reconstruction, raster = landscape_classified, significance_level = significance_level)
## > Input: randomized pattern
## > Quantile thresholds: negative < 0.005 || positive > 0.995
##   habitat count    lo    hi significance
## 1       1     6 21.96 49.02     negative
## 2       2    18 32.47 64.51     negative
## 3       3    18 26.98 56.10     negative
## 4       4    21 17.98 40.00         n.s.
## 5       5   129 24.96 52.02     positive
```

## Contributing and Code of Conduct

Contributions to **shar** are highly welcomed and appreciated. This
includes any form of feedback, bug reports, feature
requests/suggestions, or general questions about the usage.

Please note that the **shar** package is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

To see how to contribute to this project, please see the [Contributing
guidelines](CONTRIBUTING.md).

### References

Baddeley, A., Rubak, E., Turner, R., 2015. Spatial point patterns:
Methodology and applications with R. Chapman and Hall/CRC Press, London.
<isbn:978-1-4822-1020-0>

Harms, K.E., Condit, R., Hubbell, S.P., Foster, R.B., 2001. Habitat
associations of trees and shrubs in a 50-ha neotropical forest plot.
Journal of Ecology 89, 947–959.
<https://doi.org/10.1111/j.1365-2745.2001.00615.x>

Hijmans, R.J., 2019. raster: Geographic data analysis and modeling. R
package version 2.9-5. <https://cran.r-project.org/package=raster>.

Kirkpatrick, S., Gelatt, C.D.Jr., Vecchi, M.P., 1983. Optimization by
simulated annealing. Science 220, 671–680.
<https://doi.org/10.1126/science.220.4598.671>

Plotkin, J.B., Potts, M.D., Leslie, N., Manokaran, N., LaFrankie, J.V.,
Ashton, P.S., 2000. Species-area curves, spatial aggregation, and
habitat specialization in tropical forests. Journal of Theoretical
Biology 207, 81–99. <https://doi.org/10.1006/jtbi.2000.2158>

Tscheschel, A., Stoyan, D., 2006. Statistical reconstruction of random
point patterns. Computational Statistics and Data Analysis 51, 859–871.
<https://doi.org/10.1016/j.csda.2005.09.007>

Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern
analysis in ecology. Chapman and Hall/CRC Press, Boca Raton.
<isbn:978-1-4200-8254-8>

Yamada, T., Tomita, A., Itoh, A., Yamakura, T., Ohkubo, T., Kanzaki, M.,
Tan, S., Ashton, P.S., 2006. Habitat associations of Sterculiaceae trees
in a Bornean rain forest plot. Journal of Vegetation Science 17,
559–566. <https://doi.org/10.1111/j.1654-1103.2006.tb02479.x>
