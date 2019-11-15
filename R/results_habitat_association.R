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
#' \code{\link{reconstruct_pattern_homo}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}}
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
#' of trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology, 89(6), 947-959.
#'
#' Plotkin, J. B., Potts, M. D., Leslie, N., Manokaran, N., LaFrankie, J. V., & Ashton, P. S. (2000).
#' Species-area curves, spatial aggregation, and habitat specialization in tropical forests.
#' Journal of Theoretical Biology, 207(1), 81-99.

#' @export
results_habitat_association <- function(pattern, raster,
                                        significance_level = 0.05, verbose = TRUE) {

  if (class(pattern) != "rd_pat" && class(raster) != "rd_ras") {

    stop("Class of 'pattern' or 'raster' must be either 'rd_pat' or 'rd_ras'.",
         call. = FALSE)
  }

  if (class(pattern) == "rd_pat" && class(raster) == "rd_ras") {

    stop("Please provide only one randomized input.",
         call. = FALSE)
  }

  if (significance_level < 0.01 || significance_level > 0.1 && verbose) {
    warning("Make sure 'signifcance_level' is meaningful (e.g. 'significance_level = 0.05').",
            call. = FALSE)
  }

  # probs for quantile function from significance level
  threshold <- c(significance_level / 2, 1 - significance_level / 2)

  # randomized rasters as input
  if (class(raster) == "rd_ras") {

    # check if randomized and observed is present
    if (!methods::is(raster$observed, "RasterLayer")) {

      stop("The observed raster needs to be included in the input 'raster'.",
           call. = FALSE)
    }

    # check if extent is identical
    same_extent <- raster::extent(raster$observed) == raster::extent(pattern$window$xrange,
                                                                     pattern$window$yrange)

    # error if extent is not identical
    if (!same_extent) {

      warning("Extent of 'pattern' and 'raster' are not identical.", call. = FALSE)
    }

    habitats <- sort(table(raster$observed@data@values, useNA = "no")) # get table of habitats

    # print warning if more than 10 classes are present
    if (verbose) {

      if (length(habitats) > 10) {

        warning("The raster has more than 10 classes. Please make sure discrete classes are provided.",
                call. = FALSE)
      }
    }

    # print quantiles
    if (verbose) {

      message("> Input: randomized raster | Quantile thresholds: negative < ",
              threshold[1], " - positive > ", threshold[2])
    }

    # combine observed and randomized to one list again
    raster <- c(raster$randomized, list(raster$observed))

    names(raster) <- c(paste0("randomized_", seq(from = 1,
                                                 to = length(raster) - 1,
                                                 by = 1)),
                             "observed")

    # extract number of points within each habitat for all list entries
    habitats_count <- lapply(raster, function(current_raster) {

      shar::extract_points(raster = current_raster,
                           pattern = pattern)
    })
  }

  # randomized patterns as input
  else if (class(pattern) == "rd_pat") {

    # check if randomized and observed is present
    if (!spatstat::is.ppp(pattern$observed)) {

      stop("The observed pattern needs to be included in the input 'pattern'.",
           call. = FALSE)
    }

    # check if extent is identical
    same_extent <- raster::extent(raster) == raster::extent(pattern$observed$window$xrange,
                                                            pattern$observed$window$yrange)

    # error if extent is not identical
    if (!same_extent) {

      warning("Extent of 'pattern' and 'raster' are not identical.", call. = FALSE)
    }

    habitats <- sort(table(raster@data@values, useNA = "no")) # get table of habitats

    # print warning if more than 10 classes are present
    if (verbose) {

      if (length(habitats) > 10) {

        warning("The raster has more than 10 classes. Please make sure discrete classes are provided.",
                call. = FALSE)
      }
    }

    # print quantiles
    if (verbose) {
      message("> Input: randomized point pattern | Quantile thresholds: negative < ",
              threshold[1], " - positive > ", threshold[2])
    }

    # combine observed and randomized to one list again
    pattern <- c(pattern$randomized, list(pattern$observed))

    names(pattern) <- c(paste0("randomized_", seq(from = 1,
                                                  to = length(pattern) - 1,
                                                  by = 1)),
                             "observed")

    # extract number of points within each habitat for all list entries
    habitats_count <- lapply(pattern, function(current_pattern) {

      shar::extract_points(raster = raster,
                           pattern = current_pattern)
    })
  }

  # count number of habitats
  number_habitats <- length(unique(habitats_count$observed$habitat))

  # combine to one df
  names <- names(habitats_count)

  # repeat each name as often as number of habitats
  names <- rep(names, each = number_habitats)

  # rowbind to one dataframe
  habitats_count <- do.call(rbind, unname(habitats_count))

  # add id
  habitats_count$type <- names

  # only randomized data
  habitats_count_random <- habitats_count[habitats_count$type != "observed", ]

  # get quanitiles of randomized data
  habitats_count_random_summarised <- do.call(data.frame,
                                              stats::aggregate(x = data.frame(count = habitats_count_random$count),
                                                               by = data.frame(habitat = habitats_count_random$habitat),
                                                               FUN = function(x)
                                                                 cbind(lo = stats::quantile(x, probs = threshold[[1]]),
                                                                       hi = stats::quantile(x, probs = threshold[[2]]))))



  # convert to dataframe
  names(habitats_count_random_summarised) <- c("habitat", "lo", "hi")

  # get observed data
  habitats_count_obs <- habitats_count[habitats_count$type == "observed", 1:2]

  # combine (join) with quantiles of randomized data
  result <- merge(x = habitats_count_obs,
                  y = habitats_count_random_summarised,
                  by = "habitat")

  # classify results to positive/negative/n.s.
  result$significance <- ifelse(test = result$count < result$lo,
                                yes = "negative",
                                no = ifelse(test = result$count > result$hi,
                                            yes = "positive",
                                            no = "n.s."))

  return(result)
}
