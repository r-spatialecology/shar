#' results_habitat_association
#'
#' @description Results habitat association
#'
#' @param pattern ppp object with original point pattern data or rd_pat or rd_mar
#' object with randomized point pattern.
#' @param raster SpatRaster with original discrete habitat data or rd_ras object with
#' randomized environmental data.
#' @param significance_level Double with significance level.
#' @param breaks Vector with breaks of habitat classes.
#' @param digits Integer with digits used during rounding.
#' @param verbose Logical if messages should be printed.
#'
#' @details
#' The functions shows significant habitat associations by comparing the number of
#' points within a habitat between the observed data and randomized data as described in
#' Plotkin et al. (2000) and Harms et al. (2001). Significant positive or associations are present
#' if the observed count in a habitat is above or below a certain threshold of the
#' randomized count, respectively.
#'
#' In case the SpatRaster contains NA cells, this needs to be reflected in the observation
#' window of the point pattern as well (i.e., no point locations possible in these areas).
#'
#' If \code{breaks = NULL} (default), only habitat labels (but not breaks) will be
#' returned. If a vector with \code{breaks} is provided (same order as increasing habitat values),
#' the breaks will be included as well.
#'
#' @seealso
#' \code{\link{reconstruct_pattern}} \cr
#' \code{\link{fit_point_process}}
#'
#' @return data.frame
#'
#' @examples
#' landscape_classified <- classify_habitats(landscape, n = 5, style = "fisher")
#' species_a_random <- fit_point_process(species_a, n_random = 199)
#' results_habitat_association(pattern = species_a_random, raster = landscape_classified)
#'
#' @aliases results_habitat_association
#' @rdname results_habitat_association
#'
#' @references
#' Harms, K.E., Condit, R., Hubbell, S.P., Foster, R.B., 2001. Habitat associations of
#' trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology 89, 947–959.
#' <https://doi.org/10.1111/j.1365-2745.2001.00615.x>
#'
#' Plotkin, J.B., Potts, M.D., Leslie, N., Manokaran, N., LaFrankie, J.V.,
#' Ashton, P.S., 2000. Species-area curves, spatial aggregation, and habitat specialization
#' in tropical forests. Journal of Theoretical Biology 207, 81–99.
#' <https://doi.org/10.1006/jtbi.2000.2158>
#'
#' @export
results_habitat_association <- function(pattern, raster, significance_level = 0.05,
                                        breaks = NULL, digits = NULL, verbose = TRUE) {

  if (!inherits(x = pattern, what = "rd_pat") && !inherits(x = raster, what = "rd_ras")) {

    stop("Class of 'pattern' or 'raster' must be either 'rd_pat' or 'rd_ras'.",
         call. = FALSE)

  }

  if (inherits(x = pattern, what = "rd_pat") && inherits(x = raster, what = "rd_ras")) {

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
  if (inherits(x = raster, what = "rd_ras")) {

    # check if randomized and observed is present
    if (!methods::is(raster$observed, "SpatRaster")) {

      stop("The observed raster needs to be included in the input 'raster'.",
           call. = FALSE)

    }

    # check if pattern is ppp
    if (!inherits(x = pattern, what = "ppp")) {

      stop("Pleaster provide 'ppp' as pattern argument.", call. = FALSE)

    }


    # check if extent is identical
    same_extent <- terra::ext(raster$observed) == terra::ext(pattern$window$xrange,
                                                             pattern$window$yrange)

    # error if extent is not identical
    if (!same_extent) {

      warning("Extent of 'pattern' and 'raster' are not identical.", call. = FALSE)

    }

    habitats <- sort(table(raster$observed@data@values, useNA = "no")) # get table of habitats

    # print warning if more than 25 classes are present
    if (verbose) {

      if (length(habitats) > 25) {

        warning("The raster has more than 25 classes. You can ignore this warning if your raster data is discrete.",
                call. = FALSE)

      }
    }

    # print quantiles
    if (verbose) {

      message("> Input: randomized raster\n> Quantile thresholds: negative < ",
              threshold[1], " || positive > ", threshold[2])

    }

    # combine observed and randomized to one list again
    raster <- c(raster$randomized, list(raster$observed))

    names(raster) <- c(paste0("randomized_", seq(from = 1,
                                                 to = length(raster) - 1,
                                                 by = 1)),
                             "observed")

    # extract number of points within each habitat for all list entries
    habitats_count <- lapply(raster, function(current_raster) {

      extract_points(raster = current_raster, pattern = pattern)

    })
  }

  # randomized patterns as input
  else if (inherits(x = pattern, what = "rd_pat")) {

    # check if randomized and observed is present
    if (!spatstat.geom::is.ppp(pattern$observed)) {

      stop("The observed pattern needs to be included in the input 'pattern'.",
           call. = FALSE)

    }

    # check of raster is SpatRaster
    if (!inherits(x = raster, what = "SpatRaster")) {

      stop("Pleaster provide 'SpatRaster' as raster argument.", call. = FALSE)

    }

    # check if extent is identical
    same_extent <- terra::ext(raster) == terra::ext(pattern$observed$window$xrange,
                                                    pattern$observed$window$yrange)

    # error if extent is not identical
    if (!same_extent) {

      warning("Extent of 'pattern' and 'raster' are not identical.", call. = FALSE)

    }

    # warning if NA are present
    if (anyNA(terra::values(raster, mat = FALSE))) {

      warning("NA values present. Please make sure the observation window of the point pattern reflects this.", call. = FALSE)

    }

    habitats <- sort(table(terra::values(raster, mat = FALSE))) # get table of habitats

    # print warning if more than 25 classes are present
    if (verbose) {

      if (length(habitats) > 25) {

        warning("The raster has more than 25 classes. You can ignore this warning if your raster data is discrete.",
                call. = FALSE)

      }
    }

    # print quantiles
    if (verbose) {

      message("> Input: randomized pattern\n> Quantile thresholds: negative < ",
              threshold[1], " || positive > ", threshold[2])

    }

    # combine observed and randomized to one list again
    pattern <- c(pattern$randomized, list(pattern$observed))

    names(pattern) <- c(paste0("randomized_", seq(from = 1, to = length(pattern) - 1,
                                                  by = 1)),
                             "observed")

    # extract number of points within each habitat for all list entries
    habitats_count <- lapply(pattern, function(current_pattern) {

      extract_points(raster = raster, pattern = current_pattern)

    })
  }

  # repeat each name as often as number of habitats
  names_obj <- rep(x = names(habitats_count), each = length(habitats))

  # rowbind to one dataframe
  habitats_count <- do.call(rbind, unname(habitats_count))

  # add id
  habitats_count$type <- names_obj

  # only randomized data
  habitats_count_random <- habitats_count[habitats_count$type != "observed", ]

  # get quanitiles of randomized data
  habitats_count_random_summarised <-
    do.call(data.frame, stats::aggregate(x = data.frame(count = habitats_count_random$count),
                                         by = data.frame(habitat = habitats_count_random$habitat),
                                         FUN = function(x)
                                           cbind(lo = stats::quantile(x, probs = threshold[[1]]),
                                                 hi = stats::quantile(x, probs = threshold[[2]]))))

  # convert to dataframe
  names(habitats_count_random_summarised) <- c("habitat", "lo", "hi")

  # adding breaks to data.frame
  if (is.null(breaks)) {

    habitats_count_random_summarised$breaks <- rep(x = NA, times = length(habitats))

  } else {

    # check class
    if (inherits(x = breaks, what = "classIntervals")) {

      breaks <- classint_to_vector(x = breaks, digits = digits)

    }

    habitats_count_random_summarised$breaks <- breaks

  }

  # get observed data
  habitats_count_obs <- habitats_count[habitats_count$type == "observed", 1:2]

  # combine (join) with quantiles of randomized data
  result <- merge(x = habitats_count_obs, y = habitats_count_random_summarised,
                  by = "habitat")

  # classify results to positive/negative/n.s.
  result$significance <- ifelse(test = result$count < result$lo,
                                yes = "negative",
                                no = ifelse(test = result$count > result$hi,
                                            yes = "positive", no = "n.s."))

  # reorder columns
  result <- result[, c("habitat", "breaks", "count", "lo", "hi", "significance")]

  return(result)
}
