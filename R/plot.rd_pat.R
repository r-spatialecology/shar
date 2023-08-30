#' plot.rd_pat
#'
#' @description Plot method for rd_pat object
#'
#' @param x rd_pat object with randomized patterns.
#' @param what Character specifying to plot summary functions of point patterns
#' (\code{what = "sf"}) or actual patterns (\code{what = "pp"}).
#' @param n Integer with number or vector of ids of randomized pattern to plot.
#' See Details section for more information.
#' @param probs Vector with quantiles of randomized data used for envelope construction.
#' @param ask Logical if the user is asked to press <RETURN> before second summary function
#' is plotted (only used if \code{what = "sf"}).
#' @param verbose Logical if progress report is printed.
#' @param ... Not used.
#'
#' @details
#' The function plots the pair correlation function and the nearest neighbour function of
#' the observed pattern and the reconstructed patterns (as "simulation envelopes").
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
#' pattern_random <- fit_point_process(species_a, n_random = 39)
#' plot(pattern_random)
#'
#' pattern_recon <- reconstruct_pattern(species_b, n_random = 19,
#' max_runs = 1000, method = "hetero")
#' plot(pattern_recon)
#' }
#'
#' @aliases plot.rd_pat
#' @rdname plot.rd_pat
#'
#' @export
plot.rd_pat <- function(x, what = "sf", n = NULL, probs = c(0.025, 0.975),
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

    name_unit <- spatstat.geom::unitname(x$observed)[[1]] # unit name for labels

    # calculate r
    r <- seq(from = 0, to = spatstat.explore::rmax.rule(W = x$observed$window,
                                                     lambda = spatstat.geom::intensity.ppp(x$observed)),
             length.out = 250)

    # combine observed and randomized to one list again
    pattern <- c(x$randomized, list(x$observed))

    names(pattern) <- pattern_names

    # loop through all input
    result <- lapply(seq_along(pattern), function(x) {

      # calculate summary functions
      gest_result <- spatstat.explore::Gest(pattern[[x]], correction = "none", r = r)

      pcf_result <- spatstat.explore::pcf(pattern[[x]], divisor = "d",
                                         correction = "none", r = r)

      gest_df <- as.data.frame(gest_result) # conver to df

      names(gest_df)[3] <- "x_r" # unique col names

      gest_df$summary_function <- "G(r)" # name of method

      pcf_df <- as.data.frame(pcf_result) # convert to df

      names(pcf_df)[3] <- "x_r" # unique col names

      pcf_df$summary_function <- "g(r)" # name of method

      summary_stats <- rbind(gest_df, pcf_df)

      # print progress
      if (verbose) {

        message("\r> Progress: ", x, "/", length(pattern), "\t\t", appendLF = FALSE)

      }

      return(summary_stats)

    })

    # get number of rows
    result_nrow <- vapply(X = result, FUN = nrow, FUN.VALUE = numeric(1))

    # combine results to one dataframe
    result <- cbind(do.call(rbind, result), pattern = rep(pattern_names, times = result_nrow))

    # classify all observed and all randomized repetitions identical
    result_randomized <- result[result$pattern != "observed", ]
    result_observed <- result[result$pattern == "observed", ]

    # summarise randomized data
    result_randomized <- do.call(data.frame,
                                 stats::aggregate(x_r ~ summary_function + r,
                                                  data = result_randomized,
                                                  FUN = function(x){c(lo = stats::quantile(x, probs = probs[1]),
                                                                      hi = stats::quantile(x, probs = probs[2]))}))

    # results Gest
    summarised_nndf <- result_randomized[result_randomized$summary_function == "G(r)", ]

    # results pcf
    summarised_pcf <- result_randomized[result_randomized$summary_function == "g(r)", ]

    # get y scale ranges
    yrange_nndf <- c(min(c(result_observed[, 3][result_observed$summary_function == "G(r)"],
                           summarised_nndf[, 3])),
                     max(c(result_observed[, 3][result_observed$summary_function == "G(r)"],
                           summarised_nndf[, 4])))

    yrange_pcf <- c(min(c(result_observed[, 3][result_observed$summary_function == "g(r)"],
                          summarised_pcf[, 3])),
                    max(c(result_observed[, 3][result_observed$summary_function == "g(r)"],
                          summarised_pcf[, 4])))

    # specify quantums G(r)
    col_quantum <- ifelse(test = result_observed[result_observed$summary_function == "G(r)", 3] < summarised_nndf[, 3] |
                            result_observed[result_observed$summary_function == "G(r)", 3] > summarised_nndf[, 4],
                          yes = "#1f78b4", no = "#b2df8a")

    # specify quantums g(r)
    col_pcf <- ifelse(test = result_observed[result_observed$summary_function == "g(r)", 3] < summarised_pcf[, 3] |
                        result_observed[result_observed$summary_function == "g(r)", 3] > summarised_pcf[, 4],
                      yes =  "#1f78b4", no = "#b2df8a")

    # plot results G(r)

    # plot Gest
    graphics::plot(NULL, xlim = range(r), ylim = yrange_nndf,
                   main = "Nearest Neighbour Distance Function",
                   xlab = paste0("r [",name_unit, "]"), ylab = "G(r)")

    graphics::polygon(x = c(summarised_nndf[, 2], rev(x = summarised_nndf[, 2])),
                      y = c(summarised_nndf[, 3], rev(x = summarised_nndf[, 4])),
                      col = 'grey80', border = NA)

    graphics::segments(x0 = r, y0 = yrange_nndf[1] - (yrange_nndf[2] - yrange_nndf[1]) / 50,
                       y1 = yrange_nndf[1] - (yrange_nndf[2] - yrange_nndf[1]) / 25,
                       col = col_quantum, lwd = 2.5)

    graphics::lines(x = result_observed[, 1][result_observed$summary_function == "G(r)"],
                    y = result_observed[, 3][result_observed$summary_function == "G(r)"])

    graphics::lines(x = summarised_nndf[, 2], y = summarised_nndf[, 3],
                    col = "#1f78b4", lty = 2)

    graphics::lines(x = summarised_nndf[, 2], y = summarised_nndf[, 4],
                    col = "#1f78b4", lty = 2)

    graphics::legend(x = "topright", legend = c("observed", "randomized"),
                     col = c("black", "#1f78b4"), lty = c(1,2), inset = 0.025)

    # ask user to hit enter
    graphics::par(ask = ask)

    # plot pcf
    graphics::plot(NULL, xlim = range(r), ylim = yrange_pcf, main = "Pair Correlation Function",
                   xlab = paste0("r [",name_unit, "]"), ylab = "g(r)")

    graphics::polygon(x = c(summarised_pcf[, 2], rev(x = summarised_pcf[, 2])),
                      y = c(summarised_pcf[, 3], rev(x = summarised_pcf[, 4])),
                      col = 'grey80', border = NA)

    graphics::segments(x0 = r, y0 = yrange_pcf[1] - (yrange_pcf[2] - yrange_pcf[1]) / 50,
                       y1 = yrange_pcf[1] - (yrange_pcf[2] - yrange_pcf[1]) / 25,
                       col = col_pcf, lwd = 2.5)

    graphics::lines(x = result_observed[, 1][result_observed$summary_function == "g(r)"],
                    y = result_observed[, 3][result_observed$summary_function == "g(r)"])

    graphics::lines(x = summarised_pcf[, 2], y = summarised_pcf[, 3],
                    col = "#1f78b4", lty = 2)

    graphics::lines(x = summarised_pcf[, 2], y = summarised_pcf[, 4],
                    col = "#1f78b4", lty = 2)

    graphics::legend(x = "topright", legend = c("observed", "randomized"),
                     col = c("black", "#1f78b4"), lty = c(1,2), inset = 0.025)

    graphics::par(ask = FALSE)

    invisible() ####

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
