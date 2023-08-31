#' Example landscape (random cluster neutral landscape model).
#'
#' An example map to show landscapetools functionality
#' generated with the \code{NLMR::nlm_fbm()} algorithm.
#'
#' @format A SpatRaster object.
#' @source Simulated neutral landscape model with R. https://github.com/ropensci/NLMR/
"landscape"

#' Species a
#'
#' A species with negative associations to habitat 4 of \code{landscape}. Please be
#' aware that a negative association to one habitat will inevitable lead to positive
#' associations to other habitats (Yamada et al. 2006).
#'
#'@references
#' Yamada, T., Tomita, A., Itoh, A., Yamakura, T., Ohkubo, T., Kanzaki, M., Tan, S.,
#' Ashton, P.S., 2006. Habitat associations of Sterculiaceae trees in a Bornean rain
#' forest plot. Journal of Vegetation Science 17, 559–566.
#'
#' @format A spatstat ppp object.
"species_a"

#' Species b
#'
#' A species with positive associations to habitat 5 of \code{landscape}. Please be
#' aware that a positive association to one habitat will inevitable lead to negative
#' associations to other habitats (Yamada et al. 2006)
#'
#'@references
#' Yamada, T., Tomita, A., Itoh, A., Yamakura, T., Ohkubo, T., Kanzaki, M., Tan, S.,
#' Ashton, P.S., 2006. Habitat associations of Sterculiaceae trees in a Bornean rain
#' forest plot. Journal of Vegetation Science 17, 559–566.
#'
#' @format A spatstat ppp object.
"species_b"
