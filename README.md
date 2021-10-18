
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

| Continuous Integration                                                                                                                                 | Development                                                                                                                        | CRAN                                                                                                                    | License                                                                                                         |
| ------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| [![R build status](https://github.com/r-spatialecology/shar/workflows/R-CMD-check/badge.svg)](https://github.com/r-spatialecology/shar/actions)        | [![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)                       | [![CRAN status](https://www.r-pkg.org/badges/version/shar)](https://cran.r-project.org/package=shar)                    | [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) |
| [![Coverage status](https://codecov.io/gh/r-spatialecology/shar/branch/main/graph/badge.svg)](https://codecov.io/gh/r-spatialecology/shar?branch=main) | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) | [![CRAN logs](http://cranlogs.r-pkg.org/badges/grand-total/shar)](http://cran.rstudio.com/web/packages/shar/index.html) |                                                                                                                 |

<!-- badges: end -->

<img src="man/figures/logo.png" align="right" width="150" />

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
the [`spatstat`](http://spatstat.org) (Baddeley et al. 2015) and
[`raster`](https://rspatial.org/raster/) (Hijmans 2017) package.

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
examplary continuous environmental data. However, all methods depend on
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
[here](https://r-spatialecology.github.io/shar/articles/articles/background.html).

``` r
torus_trans <- translate_raster(raster = landscape_classified)

random_walk <- randomize_raster(raster = landscape_classified, n_random = 99)
```

``` r
col = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")

plot_randomized_raster(torus_trans, n = 3, col = col)
```

<img src="man/figures/README-plot_habitat-random-1.png" style="display: block; margin: auto;" />

To randomize the point pattern, either use the Gamma test described by
Plotkin et al. (2000) or pattern reconstruction (Kirkpatrick et
al. 1983; Tscheschel & Stoyan 2006).

``` r
gamma_test <- fit_point_process(pattern = species_b, process = "cluster", n_random = 99)

# (this can takes some time)
reconstruction <- reconstruct_pattern_cluster(pattern = species_b, n_random = 99, e_threshold = 0.05)
```

Of course, there are several utility functions. For example, you can
plot a randomized pattern or calculate the differences between the
observed pattern and the randomized patterns (using summary functions).

``` r
plot_randomized_pattern(reconstruction, verbose = FALSE, ask = FALSE)
```

<img src="man/figures/README-plot-random_pattern-1.png" style="display: block; margin: auto;" /><img src="man/figures/README-plot-random_pattern-2.png" style="display: block; margin: auto;" />

``` r
calculate_energy(reconstruction, verbose = FALSE)
##  randomized_1  randomized_2  randomized_3  randomized_4  randomized_5 
##    0.04766766    0.04981575    0.04924655    0.04881003    0.04994730 
##  randomized_6  randomized_7  randomized_8  randomized_9 randomized_10 
##    0.04878444    0.04996805    0.04826684    0.04939468    0.04986018 
## randomized_11 randomized_12 randomized_13 randomized_14 randomized_15 
##    0.04763519    0.04940072    0.04978252    0.04839312    0.04967860 
## randomized_16 randomized_17 randomized_18 randomized_19 randomized_20 
##    0.04871371    0.04853621    0.04979228    0.04705545    0.04680580 
## randomized_21 randomized_22 randomized_23 randomized_24 randomized_25 
##    0.04945977    0.04920609    0.04873707    0.04892163    0.04918940 
## randomized_26 randomized_27 randomized_28 randomized_29 randomized_30 
##    0.04948005    0.04701031    0.04990232    0.04868931    0.04959118 
## randomized_31 randomized_32 randomized_33 randomized_34 randomized_35 
##    0.04969287    0.04968591    0.04961149    0.04988683    0.04960771 
## randomized_36 randomized_37 randomized_38 randomized_39 randomized_40 
##    0.04919467    0.04973387    0.04680589    0.04999381    0.04806602 
## randomized_41 randomized_42 randomized_43 randomized_44 randomized_45 
##    0.04861322    0.04738086    0.04863742    0.04904364    0.04977857 
## randomized_46 randomized_47 randomized_48 randomized_49 randomized_50 
##    0.04977030    0.04990457    0.04984227    0.04985202    0.04539586 
## randomized_51 randomized_52 randomized_53 randomized_54 randomized_55 
##    0.04908810    0.04883101    0.04919266    0.04901358    0.04776478 
## randomized_56 randomized_57 randomized_58 randomized_59 randomized_60 
##    0.04733472    0.04936047    0.04848320    0.04990588    0.04815230 
## randomized_61 randomized_62 randomized_63 randomized_64 randomized_65 
##    0.04907606    0.04863940    0.04956025    0.04714224    0.04829516 
## randomized_66 randomized_67 randomized_68 randomized_69 randomized_70 
##    0.04993923    0.04939938    0.04969626    0.04839982    0.04863939 
## randomized_71 randomized_72 randomized_73 randomized_74 randomized_75 
##    0.04878619    0.04935887    0.04965428    0.04789368    0.04960437 
## randomized_76 randomized_77 randomized_78 randomized_79 randomized_80 
##    0.04969112    0.04984624    0.04918832    0.04753634    0.04820472 
## randomized_81 randomized_82 randomized_83 randomized_84 randomized_85 
##    0.04973675    0.04934650    0.04973767    0.04363837    0.04982195 
## randomized_86 randomized_87 randomized_88 randomized_89 randomized_90 
##    0.04844810    0.04932461    0.04816726    0.04817131    0.04779692 
## randomized_91 randomized_92 randomized_93 randomized_94 randomized_95 
##    0.04879128    0.04948420    0.04675751    0.04911651    0.04750497 
## randomized_96 randomized_97 randomized_98 randomized_99 
##    0.04794680    0.04822940    0.04980291    0.04979001
```

The data was created that `species_a` has a negative association to
habitat 4 and `species_b` has a positive association to habitat 5. At
one point a positive association to one habitat leads consequently to a
negative association to another habitat (and vice versa). All this can
be seen in the results.

``` r
results_habitat_association(pattern = species_a, raster = torus_trans)
## > Input: randomized raster
## > Quantile thresholds: negative < 0.025 || positive > 0.975
##   habitat count lo hi significance
## 1       1    35 11 32     positive
## 2       2    44 22 50         n.s.
## 3       3    36 18 45         n.s.
## 4       4     4 24 53     negative
## 5       5    73 51 85         n.s.

results_habitat_association(pattern = reconstruction, raster = landscape_classified)
## > Input: randomized pattern
## > Quantile thresholds: negative < 0.025 || positive > 0.975
##   habitat count   lo    hi significance
## 1       1     4  2.9 17.55         n.s.
## 2       2    10 31.0 51.55     negative
## 3       3    13 18.0 31.55     negative
## 4       4    15 22.0 36.00     negative
## 5       5    98 26.0 47.00     positive
```

Please be aware that due to the randomization of the null model data,
results might slightly differ between different randomization approaches
(e.g., `fit_point_process()` vs. `translate_raster`) and even for
repetitions of the same approach. However, general results and trends
should be similar.

## Contributing and Code of Conduct

Contributions to **shar** are highly welcomed and appreciated. This
includes any form of feedback, bug reports, feature
requests/suggestions, or general questions about the usage. Please feel
free to either open an
[issue](https://github.com/r-spatialecology/shar/issues/), contact the
authors via [mail](mailto:mhk.hesselbarth@gmail.com), or fork the repo
and raise a pull request.

Please note that the **shar** project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## References

Baddeley, A., Rubak, E., Turner, R., 2015. Spatial point patterns:
Methodology and applications with R. Chapman and Hall/CRC Press, London.
<isbn:9781482210200>

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
<isbn:9781420082548>
