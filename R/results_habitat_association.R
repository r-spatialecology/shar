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
#' \code{\link{randomize_raster}} \cr
#' \code{\link{translate_raster}} \cr
#' \code{\link{reconstruct_pattern}}
#'
#' @return data.frame
#'
#' @examples
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#' species_a_random <- fit_point_process(species_a, n_random = 199)
#' results_habitat_association(pattern = species_a_random, raster = landscape_classified)
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

  if(significance_level < 0.01 || significance_level > 0.1) {
    warning("Make sure signifcance_level is meaningful (e.g. 'significance_level = 0.05').",
            call. = FALSE)
  }

  # probs for quantile function from significance level
  threshold <- c(significance_level / 2, 1 - significance_level / 2)

  # randomized rasters as input
  if(class(raster) == "list" && class(pattern) != "list") {

    # check if randomized and observed is present
    if(!all(c(paste0("randomized_", seq_len(length(raster) - 1)), "observed") == names(raster)) || is.null(names(raster))) {
      stop("Input must named 'randomized_1' to 'randomized_n' and includ 'observed' raster.",
           call. = FALSE)
    }

    # print quantiles
    if(verbose){
      message("> Input: randomized raster | Quantile thresholds: negative < ",
              threshold[1], " - positive > ", threshold[2])
    }

    # extract number of points within each habitat for all list entries
    habitats_count <- lapply(raster, function(current_raster) {

      shar::extract_points(raster = current_raster,
                           pattern = pattern)
    })
  }

  # randomized patterns as input
  else if(class(pattern) == "list" && class(raster) != "list") {

    # check if randomized and observed is present
    if(!all(c(paste0("randomized_", seq_len(length(pattern) - 1)), "observed") == names(pattern)) || is.null(names(pattern))) {
      stop("Input must named 'randomized_1' to 'randomized_n' and includ 'observed' raster.",
           call. = FALSE)
    }

    # print quantiles
    if(verbose){
      message("> Input: randomized point pattern | Quantile thresholds: negative < ",
              threshold[1], " - positive > ", threshold[2])
    }

    # extract number of points within each habitat for all list entries
    habitats_count <- lapply(pattern, function(current_pattern) {

      shar::extract_points(raster = raster,
                           pattern = current_pattern)
    })
  }

  else{
    stop("Please provide either randomized point patterns or randomized rasters.",
         call. = FALSE)
  }

  # combine to one df
  habitats_count <- dplyr::bind_rows(habitats_count, .id = "type")

  # only randomized data
  habitats_count_random <- dplyr::filter(habitats_count, type != "observed")

  # group by habitat to summarise
  habitats_count_random_grouped <- dplyr::group_by(habitats_count_random, habitat)

  # get quanitiles of randomized data
  habitats_count_random_summarised <- dplyr::summarise(habitats_count_random_grouped,
                                                       lo = stats::quantile(count, probs = threshold[[1]]),
                                                       hi = stats::quantile(count, probs = threshold[[2]]))

  # get observed data
  habitats_count_obs <- dplyr::select(dplyr::filter(habitats_count, type == "observed"), - type)

  # combine (join) with quantiles of randomized data
  result <- dplyr::full_join(habitats_count_obs, habitats_count_random_summarised,
                             by = "habitat")

  result <- result[, c("habitat", "count", "lo", "hi")] # set col names

  # classify results to positive/negative/n.s.
  result <- dplyr::mutate(result,
                          significance = factor(dplyr::case_when(count < lo ~ "negative",
                                                                 count > hi ~ "positive",
                                                                 count>= lo & count <= hi ~ "n.s.")))

  return(result)
}
