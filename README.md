
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **shar** \| **S**pecies **h**abitat **a**ssociations in **R** <img src="man/figures/logo.png" align="right" alt="" width="150" />

<!-- badges: start -->

README Last updated: 2023-08-31

| CI                                                                                                                                                                                   | Development                                                                                                                | CRAN                                                                                                                    | License                                                                                                                                              |
|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| [![R-CMD-check](https://github.com/r-spatialecology/shar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-spatialecology/shar/actions/workflows/R-CMD-check.yaml) | [![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)               | [![CRAN status](https://www.r-pkg.org/badges/version/shar)](https://cran.r-project.org/package=shar)                    | [![JOSS](https://joss.theoj.org/papers/1b786c028a5425858cb0e5428bd9173b/status.svg)](https://joss.theoj.org/papers/1b786c028a5425858cb0e5428bd9173b) |
| [![codecov](https://codecov.io/gh/r-spatialecology/shar/branch/main/graph/badge.svg?token=XMo844ABs4)](https://codecov.io/gh/r-spatialecology/shar)                                  | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) | [![CRAN logs](http://cranlogs.r-pkg.org/badges/grand-total/shar)](http://cran.rstudio.com/web/packages/shar/index.html) | [![License](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)                                              |

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
Methods are described in Plotkin et al. (2000), Harms et al. (2001) and
Wiegand & Moloney (2014). **shar** is mainly based on the
[`spatstat`](http://spatstat.org) (Baddeley et al. 2015) and
[`terra`](https://rspatial.org/terra/) (Hijmans 2022) package.

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
# install.packages("remotes")
remotes::install_github("r-spatialecology/shar")
```

This also automatically installs all non-base `R` package dependencies,
namely the following packages: `classInt`, `raster`, `spatstat.explore`,
`spatstat.model`, `spatstat.geom`.

## How to use **shar**

Please refer to `vignette("Get started")` to get an introduction to
**shar**.

## Contributing and Code of Conduct

Contributions to **shar** are highly welcomed and appreciated. This
includes any form of feedback, bug reports, feature
requests/suggestions, or general questions about the usage.

Please note that the **shar** package is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

To contribute to this project, please see the [Contributing
guidelines](CONTRIBUTING.md).
