
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/r-spatialecology/shar.svg?branch=master)](https://travis-ci.org/r-spatialecology/shar)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/08hgwkr82pqb6ykq/branch/master?svg=true)](https://ci.appveyor.com/project/mhesselbarth/shar/branch/master)
[![Coverage
status](https://codecov.io/gh/r-spatialecology/shar/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatialecology/shar?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/shar)](https://cran.r-project.org/package=shar)
[![](http://cranlogs.r-pkg.org/badges/grand-total/shar)](http://cran.rstudio.com/web/packages/shar/index.html)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# shar

**S**pecies-**H**abitat **A**ssociations in **R** is a `R` package to
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
the `spatstat` (Baddeley et al. 2015) and `raster` (Hijmans 2017)
package.

## Installation

You can install the released version of **shar** from
[CRAN](https://cran.r-project.org/web/packages/shar/index.html) with:

``` r
install.packages("shar")
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

**shar** comes with build-in example data sets. `species_a` and
`species_b` are examplary location of species, e.g. trees, as
`ppp`-objects from the `spatstat` package. `landscape` contains
examplary continious environmental data. However, all methods depend on
discrete data. Therefore we need to classify the data first.

``` r
landscape_classified <- classify_habitats(raster = landscape, classes = 5)
```

There are two possibilities to randomize the environmental data, both
described in Harms et al. (2001). The first shifts the habitat map in
all 4 cardinal directions around a torus. The second one assigns the
habitat values to an empty map using a random walk algorithm. Both
functions return a list with randomized rasters and the observed one.
For more information on the methods, please click
[here](https://r-spatialecology.github.io/shar/articles/background.html).

``` r
torus_trans <- translate_raster(raster = landscape_classified, verbose = FALSE)

random_walk <- randomize_raster(raster = landscape_classified, n_random = 39, verbose = FALSE)
```

``` r
col = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")

plot_randomized_raster(torus_trans, n = 3, col = col)
```

<img src="man/figures/README-plot_habitat-random-1.png" style="display: block; margin: auto;" />

To randomize the point pattern, either use the Gamma test described by
Plotkin et al. (2000) or pattern reconstruction (Tscheschel & Stoyan
2006).

``` r
gamma_test <- fit_point_process(pattern = species_a, process = "cluster", n_random = 39, verbose = FALSE)

reconstruction <- reconstruct_pattern(pattern = species_b, n_random = 39, verbose = FALSE) # takes some time
```

Of coures, there are several utility functions. For example, you can
plot a randomized pattern or calculate the differences between the
observed pattern and the randomized patterns (using summary functions).

``` r
plot_randomized_pattern(reconstruction, verbose = FALSE, ask = FALSE)
```

<img src="man/figures/README-plot-random_pattern-1.png" style="display: block; margin: auto;" /><img src="man/figures/README-plot-random_pattern-2.png" style="display: block; margin: auto;" />

``` r
calculate_energy(reconstruction, verbose = FALSE)
#>  [1] 0.01356462 0.01867389 0.01455831 0.01754614 0.01858267 0.02008615
#>  [7] 0.01544740 0.01470591 0.01255234 0.01837727 0.01622154 0.01765028
#> [13] 0.01517141 0.02075634 0.01836698 0.01523319 0.01832501 0.02072956
#> [19] 0.01845929 0.01733616 0.01639753 0.01887276 0.01738685 0.01361427
#> [25] 0.01619153 0.01695853 0.01856459 0.01692591 0.01986748 0.01604911
#> [31] 0.01972613 0.01518272 0.01681647 0.02010567 0.01472619 0.01794340
#> [37] 0.01740046 0.01531350 0.01869501
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
#> 1       1     9  2 14         n.s.
#> 2       2    25  9 24     positive
#> 3       3    27 11 26     positive
#> 4       4     0 11 29     negative
#> 5       5    12  5 18         n.s.

results_habitat_association(pattern = reconstruction, raster = landscape_classified)
#> > Input: randomized point pattern | Quantile thresholds: negative < 0.025 - positive > 0.975
#>   habitat count    lo    hi significance
#> 1       1     8  4.85 43.15         n.s.
#> 2       2    22 26.80 68.10     negative
#> 3       3    33 46.80 74.10     negative
#> 4       4    19 29.80 69.10     negative
#> 5       5   118  9.95 37.05     positive
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
