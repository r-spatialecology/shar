#' Example landscape (random cluster neutral landscape model).
#'
#' An example map to show landscapetools functionality
#' generated with the `nlm_fbm()` algorithm.
#'
#' @format A raster layer object.
#' @source Simulated neutral landscape model with R. https://github.com/ropensci/NLMR/
"landscape"

#' Species a
#'
#' A species with negative associations to habitat 4 of `landscape`.
#'
#' @format A spatstat ppp object.
"species_a"

#' Species b
#'
#' A species with positive associations to habitat 5 of `landscape`.
#'
#' @format A spatstat ppp object.
"species_b"

#' Gamma test
#'
#' Randomized data for species b using the gamma test.
#'
#' @format rd_pat object.
"gamma_test"

#' Reconstruction
#'
#' Randomized data for species b using pattern reconstruction.
#'
#' @format rd_pat object.
"reconstruction"

#' Torus trans
#'
#' Torus translation of the classified landscape data.
#'
#' @format rd_ras object.
"torus_trans"

#' Random walk
#'
#' Randomization of the landscape data using the habitat randomization algorithm.
#'
#' @format rd_ras object.
"random_walk"
