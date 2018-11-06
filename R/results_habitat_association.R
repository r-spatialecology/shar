#' results_habitat_association
#'
#' @description Results habitat association
#'
#' @param pattern Point pattern or list with reconstructed patterns.
#' @param raster RasterLayer or list of RasterLayers.
#' @param significance_level Significance level
#' @param verbose Print output
#'
#' @details
#' The functions shows significant habitat associations by comparing the number of
#' points within a habitat between the observed data and randomized data as described in
#' Plotkin et al. (2000) and Harms et al. (2001). Significant positive or associations are present
#' if the observed count in a habitat is above or below a certain threshold of the
#' randomized count, respectively.
#'
#' @seealso
#' \code{\link{randomization_algorithm}} \cr
#' \code{\link{reconstruct_pattern}}
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, user_seed = 1)
#' landscape_classified <- SHAR::classify_habitats(landscape, classes = 5)
#' species_1 <- spatstat::runifpoint(n = 50, win = spatstat::owin(c(0, 50), c(0, 50)))
#' species_1_reconstructed <- SHAR::reconstruct_pattern(pattern = species_1, n_random = 9, max_runs = 1000)
#' results_habitat_association(pattern = species_1_reconstructed,
#' raster = landscape_classified)
#' }
#'
#' @aliases results_habitat_association
#' @rdname results_habitat_association
#'
#' @references
#' Harms, K. E., Condit, R., Hubbell, S. P., & Foster, R. B. (2001). Habitat associations
#' of trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology, 89(6), 947–959.
#'
#' Plotkin, J. B., Potts, M. D., Leslie, N., Manokaran, N., LaFrankie, J. V., & Ashton, P. S. (2000).
#' Species-area curves, spatial aggregation, and habitat specialization in tropical forests.
#' Journal of Theoretical Biology, 207(1), 81–99.

#' @export
results_habitat_association <- function(pattern, raster,
                                        significance_level = 0.05, verbose = TRUE){

  threshold <- c(significance_level / 2, 1 - significance_level / 2)

  if(class(raster) == "list" && class(pattern) != "list") {

    if(isTRUE(verbose)){
      cat(paste0("> Input: randomized raster | Thresholds: negative < ",
                 threshold[1], " - positive > ", threshold[2], "\n\n"))
    }

    habitats_count <- lapply(raster, function(current_raster) {
      SHAR::extract_points(raster = current_raster,
                           pattern = pattern)
    })
  }

  else if(class(pattern) == "list" && class(raster) != "list") {

    if(isTRUE(verbose)){
      cat(paste0("> Input: randomized point pattern | Thresholds: negative < ",
                 threshold[1], " - positive > ", threshold[2], "\n\n"))
    }

    habitats_count <- lapply(pattern, function(current_pattern) {
      SHAR::extract_points(raster = raster,
                           pattern = current_pattern)
    })
  }

  else{
    stop("Please provide either randomized point patterns or randomized rasters")
  }

  habitats_count <- dplyr::bind_rows(habitats_count, .id = "type")

  habitats_count_random <- dplyr::filter(habitats_count, type != "observed")

  habitats_count_random_grouped <- dplyr::group_by(habitats_count_random, habitat)

  habitats_count_random_summarised <- dplyr::summarise(habitats_count_random_grouped,
                                                       lo = quantile(count,probs = threshold[[1]]),
                                                       hi = quantile(count,probs = threshold[[2]]))

  habitats_count_obs <- dplyr::select(dplyr::filter(habitats_count, type == "observed"), - type)

  result <- dplyr::full_join(habitats_count_obs, habitats_count_random_summarised,
                             by = "habitat")

  result <- result[, c("habitat", "count", "lo", "hi")]

  result <- dplyr::mutate(result,
                          significance = factor(dplyr::case_when(count < lo ~ "negative",
                                                                 count > hi ~ "positive",
                                                                 count>= lo & count <= hi ~ "n.s.")))

  return(result)

}
