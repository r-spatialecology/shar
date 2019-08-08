#' plot_randomized_pattern
#'
#' @description Plot randomized pattern
#'
#' @param pattern List with reconstructed patterns.
#' @param what Plot summary functions of point patterns (\code{what = "sf"}) or acutal patterns (\code{what = "pp"}).
#' @param probs Quantiles of randomized data used for envelope construction.
#' @param comp_fast If pattern contains more points than threshold, summary functions are estimated in a computational fast way.
#' @param ask If TRUE the user is asked to press <RETURN> before second summary function
#' is plotted (only has influence if \code{what = "sf"} and \code{method = "spatial"}).
#' @param verbose Print progress report.
#'
#' @details
#' The function plots the pair correlation function and the nearest neighbour function
#' the observed pattern and the reconstructed patterns (as "simulation envelopes".).
#' For large patterns \code{comp_fast = TRUE} decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}. It is also
#' possible to plot 3 randomized patterns and the observed pattern using \code{what = "pp"}.
#'
#' @examples
#' pattern_random <- fit_point_process(species_a, n_random = 19, process = "cluster")
#' plot_randomized_pattern(pattern_random)
#'
#' plot_randomized_pattern(pattern_random, what = "pp")
#'
#' \dontrun{
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_random$randomized[[1]], marks_sub,
#' n_random = 19, max_runs = 1000)
#' plot_randomized_pattern(marks_recon)
#' }
#'
#' @aliases plot_randomized_pattern
#' @rdname plot_randomized_pattern
#'
#' @export
plot_randomized_pattern <- function(pattern,
                                    what = "sf",
                                    probs = c(0.025, 0.975),
                                    comp_fast = 1000,
                                    ask = TRUE,
                                    verbose = TRUE){

  # check if class is correct
  if (!class(pattern) %in% c("rd_pat", "rd_mar")) {

    stop("Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
         call. = FALSE)
  }

  # check if observed pattern is present
  if (!spatstat::is.ppp(pattern$observed)) {

    stop("Input must include 'observed' pattern.", call. = FALSE)
  }

  pattern_names <- c(paste0("randomized_", seq(from = 1,
                                               to = length(pattern$randomized),
                                               by = 1)),
                     "observed")

  if (what == "sf") {

    # check if number of points exceed comp_fast limit
    if (pattern$observed$n > comp_fast) {

      comp_fast <- TRUE
    }

    else {
      comp_fast <- FALSE
    }

    name_unit <- spatstat::unitname(pattern$observed)[[1]] # unit name for labels

    # calculate r
    r <- seq(from = 0,
             to = spatstat::rmax.rule(W = pattern$observed$window,
                                      lambda = spatstat::intensity.ppp(pattern$observed)),
             length.out = 250)

    if (class(pattern) == "rd_pat") {

      # combine observed and randomized to one list again
      pattern <- c(pattern$randomized, list(pattern$observed))

      names(pattern) <- pattern_names

      # loop through all input
      result <- lapply(seq_along(pattern), function(x) {

        # calculate summary functions
        if (comp_fast) {

          gest_result <- spatstat::Gest(pattern[[x]],
                                        correction = "none",
                                        r = r)

          pcf_result <- shar::estimate_pcf_fast(pattern[[x]],
                                                correction = "none",
                                                method = "c",
                                                spar = 0.5,
                                                r = r)
        }

        else {
          gest_result <- spatstat::Gest(pattern[[x]],
                                        correction = "han",
                                        r = r)

          pcf_result <- spatstat::pcf(pattern[[x]],
                                      divisor = "d",
                                      correction = "best",
                                      r = r)
        }

        gest_df <- as.data.frame(gest_result) # conver to df

        names(gest_df)[3] <- "x_r" # unique col names

        gest_df$summary_function <- "G(r)" # name of method

        pcf_df <- as.data.frame(pcf_result) # convert to df

        names(pcf_df)[3] <- "x_r" # unique col names

        pcf_df$summary_function <- "g(r)" # name of method

        summary_stats <- rbind(gest_df, pcf_df)

        # print progress
        if (verbose) {
          message("\r> Progress: ", x, "/", length(pattern), "\t\t",
                  appendLF = FALSE)
        }

        return(summary_stats)
      })

      # get number of rows
      result_nrow <- vapply(X = result,
                            FUN = nrow, FUN.VALUE = numeric(1))

      # combine results to one dataframe
      result <- cbind(do.call(rbind, result),
                      pattern = rep(pattern_names, times = result_nrow))

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

      # specify quantums G(r)
      col_quantum <- ifelse(test = result_observed[result_observed$summary_function == "G(r)", 3] < summarised_nndf[, 3] |
                              result_observed[result_observed$summary_function == "G(r)", 3] > summarised_nndf[, 4],
                            yes = "#1f78b4",
                            no = "#b2df8a")

      # specify quantums g(r)
      col_pcf <- ifelse(test = result_observed[result_observed$summary_function == "g(r)", 3] < summarised_pcf[, 3] |
                          result_observed[result_observed$summary_function == "g(r)", 3] > summarised_pcf[, 4],
                        yes =  "#1f78b4",
                        no = "#b2df8a")

      # plot results G(r)

      # plot Gest
      graphics::plot(NULL,
                     xlim = range(r),
                     ylim = c(min(summarised_nndf[, 3]) - (max(summarised_nndf[, 4]) - min(summarised_nndf[, 3])) / 25,
                              max(summarised_nndf[, 4])),
                     main = "Nearest Neighbour Distance Function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "G(r)")

      graphics::polygon(x = c(summarised_nndf[, 2], rev(x = summarised_nndf[, 2])),
                        y = c(summarised_nndf[, 3], rev(x = summarised_nndf[, 4])),
                        col = 'grey80', border = NA)

      graphics::segments(x0 = r,
                         y0 = min(summarised_nndf[, 3]) - (max(summarised_nndf[, 4]) - min(summarised_nndf[, 3])) / 50,
                         y1 = min(summarised_nndf[, 3]) - (max(summarised_nndf[, 4]) - min(summarised_nndf[, 3])) / 25,
                         col = col_quantum, lwd = 2.5)

      graphics::lines(x = result_observed[, 1][result_observed$summary_function == "G(r)"],
                      y = result_observed[, 3][result_observed$summary_function == "G(r)"])

      graphics::lines(x = summarised_nndf[, 2],
                      y = summarised_nndf[, 3],
                      col = "#1f78b4", lty = 2)

      graphics::lines(x = summarised_nndf[, 2],
                      y = summarised_nndf[, 4],
                      col = "#1f78b4", lty = 2)

      graphics::legend(x = "topright",
                       legend = c("observed", "randomized"),
                       col = c("black", "#1f78b4"), lty = c(1,2), inset = 0.025)

      # ask user to hit enter
      graphics::par(ask = ask)

      # plot pcf
      graphics::plot(NULL,
                     xlim = range(r),
                     ylim = c(min(summarised_nndf[, 3]) - (max(summarised_nndf[, 4]) - min(summarised_nndf[, 3])) / 25,
                              max(summarised_pcf[, 4])),
                     main = "Pair Correlation Function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "g(r)")

      graphics::polygon(x = c(summarised_pcf[, 2], rev(x = summarised_pcf[, 2])),
                        y = c(summarised_pcf[, 3], rev(x = summarised_pcf[, 4])),
                        col = 'grey80', border = NA)

      graphics::segments(x0 = r,
                         y0 = min(summarised_pcf[, 3]) - (max(summarised_pcf[, 4]) - min(summarised_pcf[, 3])) / 50,
                         y1 = min(summarised_pcf[, 3]) - (max(summarised_pcf[, 4]) - min(summarised_pcf[, 3])) / 25,
                         col = col_pcf, lwd = 2.5)

      graphics::lines(x = result_observed[, 1][result_observed$summary_function == "g(r)"],
                      y = result_observed[, 3][result_observed$summary_function == "g(r)"])

      graphics::lines(x = summarised_pcf[, 2], y = summarised_pcf[, 3],
                      col = "#1f78b4", lty = 2)

      graphics::lines(x = summarised_pcf[, 2], y = summarised_pcf[, 4],
                      col = "#1f78b4", lty = 2)

      graphics::legend(x = "topright",
                       legend = c("observed", "randomized"),
                       col = c("black", "#1f78b4"), lty = c(1,2), inset = 0.025)

      graphics::par(ask = FALSE)

      invisible()
    }

    else if (class(pattern) == "rd_mar") {

      # combine observed and randomized to one list again
      pattern <- c(pattern$randomized, list(pattern$observed))

      names(pattern) <- pattern_names

      result <- lapply(seq_along(pattern), function(x) {

        mark_corr <- as.data.frame(spatstat::markcorr(pattern[[x]],
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
      result_nrow <- vapply(X = result,
                            FUN = nrow, FUN.VALUE = numeric(1))

      # combine results to one dataframe
      result <- cbind(do.call(rbind, result),
                      pattern = rep(pattern_names, times = result_nrow))

      # classify all observed and all randomized repetitions identical
      result_observed <- result[result$pattern == "observed", ]
      result_randomized <- result[result$pattern != "observed", ]

      # calculate envelopes
      result_randomized <- do.call(data.frame,
                                   stats::aggregate(iso ~ r,
                                                    data = result_randomized,
                                                    FUN = function(x){c(lo = stats::quantile(x, probs = probs[1]),
                                                                        hi = stats::quantile(x, probs = probs[2]))}))

      # specify quantums g(r)
      col_kmmr <- ifelse(test = result_observed[, 3] < result_randomized[, 2] |
                           result_observed[, 3] > result_randomized[, 3],
                         yes =  "#1f78b4",
                         no = "#b2df8a")

      # plot results
      graphics::plot(NULL,
                     xlim = range(r),
                     ylim = c(min(result_randomized[, 2]) - (max(result_randomized[, 3]) - min(result_randomized[, 2])) / 25,
                              max(result_randomized[, 3])),
                     main = "Mark correlation function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "kmm(r)")

      graphics::segments(x0 = r,
                         y0 = min(result_randomized[, 2]) - (max(result_randomized[, 3]) - min(result_randomized[, 3])) / 50,
                         y1 = min(result_randomized[, 2]) - (max(result_randomized[, 3]) - min(result_randomized[, 3])) / 25,
                         col = col_kmmr, lwd = 2.5)

      graphics::polygon(x = c(result_randomized[, 1], rev(result_randomized[, 1])),
                        y = c(result_randomized[, 2], rev(result_randomized[, 3])),
                        col = "grey80", border = NA)

      graphics::lines(x = result_observed[, 1], y = result_observed[, 3])

      graphics::lines(x = result_randomized[, 1], y = result_randomized[, 2],
                      col = "#1f78b4", lty = 2)

      graphics::lines(x = result_randomized[, 1],
                      y = result_randomized[, 3],
                      col = "#1f78b4", lty = 2)

      graphics::legend(x = "topright",
                       legend = c("observed", "randomized"),
                       col = c("black", "#1f78b4"), lty = c(1, 2), inset = 0.025)

      invisible()
    }

    else{
      stop("'method' must be either 'method = 'spatial' or 'method = 'marks'.",
           call. = FALSE)
    }
  }

  else if (what == "pp") {

    # check if observed pattern is present
    if (!spatstat::is.ppp(pattern$observed)) {

      stop("Input must include 'observed' pattern.", call. = FALSE)
    }

    # get number of patterns
    number_patterns <- length(pattern$randomized)

    # check if at least 4 patterns are present, i.e. 3 randomized & observed
    if (number_patterns < 3) {

      stop("Please provide at least 3 randomizations and the observed pattern.",
           call. = FALSE)
    }

    if (verbose) {

      message("> Plotting observed pattern and 4 randomized patterns only")
    }

    # sample 3 randomized patterns
    random_ids <- shar::rcpp_sample(x = seq(from = 1, to = number_patterns, by = 1),
                                    n = 3)

    # get randomized and observed patterns
    pattern_randomized <- pattern$randomized[random_ids]
    pattern <- c(pattern_randomized, observed = list(pattern$observed))

    # get names for plot main title
    names_pattern <- names(pattern)

    # get range of observed pattern window
    x_range <- pattern$observed$window$xrange
    y_range <- pattern$observed$window$yrange
    current_window <- pattern$observed$window

    # plot 4 plots next to each other
    graphics::par(mfrow = c(2, 2))

    lapply(seq_along(pattern), function(x) {

      # convert to dataframe
      current_pattern <- as.data.frame(pattern[[x]])

      # plot points
      graphics::plot(x = current_pattern$x,
                     y = current_pattern$y,
                     type = "p",
                     asp = 1, xlim = x_range, ylim = y_range,
                     main  = names_pattern[[x]],
                     xlab = "x coordinate", ylab = "y coordinate")

      # add window
      graphics::plot(current_window, add = TRUE)
    })

    # reset plotting window settings
    graphics::par(mfrow = c(1, 1))

    invisible()
  }

  else{
    stop("Please select either what = 'sf' or what = 'pp'.", call. = FALSE)
  }
}
