#' results_habitat_association
#'
#' @description Results habitat association
#'
#' @param pattern Point pattern or list with reconstructed patterns
#' @param raster RasterLayer or list of RasterLayers
#' @param threshold Significance thresholds
#'
#' @details
#' Results of habitat association tests
#'
#' @seealso
#' \code{\link{calculate_mean_energy}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pattern_recon <- SHAR::reconstruct_pattern(pattern_random, n_random = 9, max_runs = 1000)
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
results_habitat_association <- function(pattern, raster, threshold=c(0.025, 0.975)){


  if(class(raster) == "list" && class(pattern) != "list") {

    habitats_count <- lapply(raster, function(current_raster) {
      SHAR::extract_points(raster = current_raster,
                           pattern = pattern,
                           method = method)
    })
  }

  else if(class(pattern) == "list" && class(raster) != "list") {

    habitats_count <- lapply(pattern, function(current_pattern) {
      SHAR::extract_points(raster = raster,
                           pattern = current_pattern,
                           method = method)
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
