#' plot.rd_mar
#'
#' @description Plot method for rd_pat object
#'
#' @param x rd_mar object with randomized patterns.
#' @param what Character specifying to plot summary functions of point patterns
#' (\code{what = "sf"}) or actual patterns (\code{what = "pp"}).
#' @param n Integer with number or vector of ids of randomized pattern to plot.
#' See Details section for more information.
#' @param probs Vector with quantiles of randomized data used for envelope construction.
#' @param comp_fast Integer with threshold at which summary functions are estimated
#' in a computational fast way.
#' @param ask Logical if the user is asked to press <RETURN> before second summary function
#' is plotted (only used if \code{what = "sf"}).
#' @param verbose Logical if progress report is printed.
#' @param ... Not used.
#'
#' @details
#' The function plots the pair correlation function and the nearest neighbour function of
#' the observed pattern and the reconstructed patterns (as "simulation envelopes").
#' For large patterns \code{comp_fast = TRUE} decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' It is also possible to plot n randomized patterns and the observed pattern
#' using \code{what = "pp"}. If \code{n} is a single number, \code{n} randomized
#' patterns will be sampled to plot. If \code{n} is a vector, the corresponding patterns
#' will be plotted.
#'
#' @seealso
#' \code{\link{reconstruct_pattern}} \cr
#' \code{\link{fit_point_process}}
#'
#' @return void
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern(species_a, n_random = 1, max_runs = 1000,
#' simplify = TRUE, return_input = FALSE)
#' marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_recon, marks_sub,
#' n_random = 19, max_runs = 1000)
#' plot(marks_recon)
#' }
#'
#'
#' @aliases plot.rd_mar
#' @rdname plot.rd_mar
#'
#' @export
plot.rd_mar <- function(x, what = "sf", n = NULL, probs = c(0.025, 0.975), comp_fast = 1000,
                        ask = TRUE, verbose = TRUE, ...) {

  # check if class is correct
  if (!inherits(x = x, what = c("rd_pat", "rd_mar"))) {

    stop("Class of 'pattern' must be 'rd_pat' or 'rd_mar'.", call. = FALSE)

  }

  # check if observed pattern is present
  if (!spatstat.geom::is.ppp(x$observed)) {

    stop("Input must include 'observed' pattern.", call. = FALSE)

  }

  pattern_names <- c(paste0("randomized_", seq(from = 1, to = length(x$randomized),
                                               by = 1)), "observed")

  if (what == "sf") {

    # check if number of points exceed comp_fast limit
    if (x$observed$n > comp_fast) {

      comp_fast <- TRUE

    } else {

      comp_fast <- FALSE

    }

    name_unit <- spatstat.geom::unitname(x$observed)[[1]] # unit name for labels

    # calculate r
    r <- seq(from = 0, to = spatstat.explore::rmax.rule(W = x$observed$window,
                                                     lambda = spatstat.geom::intensity.ppp(x$observed)),
             length.out = 250)

    # combine observed and randomized to one list again
    pattern <- c(x$randomized, list(x$observed))

    names(pattern) <- pattern_names

    result <- lapply(seq_along(pattern), function(x) {

      mark_corr <- as.data.frame(spatstat.explore::markcorr(pattern[[x]],
                                                         correction = "Ripley",
                                                         r = r))

      # print progress
      if (verbose) {

        message("\r> Progress: ", x, "/", length(pattern),  "\t\t",
                appendLF = FALSE)

      }

      return(mark_corr)

    })

    # get number of rows
    result_nrow <- vapply(X = result, FUN = nrow, FUN.VALUE = numeric(1))

    # combine results to one dataframe
    result <- cbind(do.call(rbind, result), pattern = rep(pattern_names, times = result_nrow))

    # classify all observed and all randomized repetitions identical
    result_observed <- result[result$pattern == "observed", ]
    result_randomized <- result[result$pattern != "observed", ]

    # calculate envelopes
    result_randomized <- do.call(data.frame,
                                 stats::aggregate(iso ~ r,
                                                  data = result_randomized,
                                                  FUN = function(x){c(lo = stats::quantile(x, probs = probs[1]),
                                                                      hi = stats::quantile(x, probs = probs[2]))}))

    # get y scale ranges
    yrange <- c(min(c(result_observed[, 3], result_randomized[, 2])),
                max(c(result_observed[, 3], result_randomized[, 3])))

    # specify quantums g(r)
    col_kmmr <- ifelse(test = result_observed[, 3] < result_randomized[, 2] |
                         result_observed[, 3] > result_randomized[, 3],
                       yes =  "#1f78b4",
                       no = "#b2df8a")

    # plot results
    graphics::plot(NULL, xlim = range(r), ylim = yrange,
                   main = "Mark correlation function",
                   xlab = paste0("r [",name_unit, "]"), ylab = "kmm(r)")

    graphics::polygon(x = c(result_randomized[, 1], rev(result_randomized[, 1])),
                      y = c(result_randomized[, 2], rev(result_randomized[, 3])),
                      col = "grey80", border = NA)

    graphics::lines(x = result_observed[, 1], y = result_observed[, 3])

    graphics::lines(x = result_randomized[, 1], y = result_randomized[, 2],
                    col = "#1f78b4", lty = 2)

    graphics::lines(x = result_randomized[, 1],
                    y = result_randomized[, 3],
                    col = "#1f78b4", lty = 2)

    graphics::segments(x0 = r, y0 = yrange[1] - (yrange[2] - yrange[1]) / 50,
                       y1 = yrange[1] - (yrange[2] - yrange[1]) / 25,
                       col = col_kmmr, lwd = 2.5)

    graphics::legend(x = "topright",
                     legend = c("observed", "randomized"),
                     col = c("black", "#1f78b4"), lty = c(1, 2), inset = 0.025)

    invisible()

  } else if (what == "pp") {

    # get randomized pattern
    subset_pattern <- sample_randomized(randomized = x$randomized, n = n,
                                        verbose = verbose)

    # get randomized and observed patterns
    subset_pattern$observed <- x$observed

    # get names for plot main title
    names_pattern <- names(subset_pattern)

    # get range of observed pattern window
    x_range <- subset_pattern$observed$window$xrange
    y_range <- subset_pattern$observed$window$yrange
    current_window <- subset_pattern$observed$window

    # get number of cols and rows
    n_col <- ceiling(sqrt(length(subset_pattern)))
    n_row <- ceiling(length(subset_pattern) / n_col)

    # set graphics options
    graphics::par(mfrow = c(n_row, n_col), mar = c(0.0, 0.0, 1.0, 0.0))

    lapply(seq_along(subset_pattern), function(i) {

      # convert to dataframe
      current_pattern <- as.data.frame(subset_pattern[[i]])

      # plot points
      graphics::plot(x = current_pattern$x, y = current_pattern$y,
                     type = "p", asp = 1, xlim = x_range, ylim = y_range, axes = FALSE,
                     main  = names_pattern[[i]], xlab = "", ylab = "")

      # add window
      graphics::plot(current_window, add = TRUE)

    })

    # reset plotting window settings
    graphics::par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

    invisible()

  } else {

    stop("Please select either what = 'sf' or what = 'pp'.", call. = FALSE)

  }
}
