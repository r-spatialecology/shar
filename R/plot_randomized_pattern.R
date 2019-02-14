#' plot_randomized_pattern
#'
#' @description Plot randomized pattern
#'
#' @param pattern List with reconstructed patterns.
#' @param what Plot summary functions of point patterns (\code{what = "sf"}) or acutal patterns (\code{what = "pp"}).
#' @param method String to specifiy if spatial pattern or marks were reconstructed
#' @param probs Quantiles of randomized data used for envelope construction.
#' @param comp_fast If pattern contains more points than threshold, summary functions are estimated in a computational fast way.
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
#' @return ggplot
#'
#' @examples
#' pattern_random <- fit_point_process(species_a, n_random = 19, process = "cluster")
#' plot_randomized_pattern(pattern_random)
#'
#' plot_randomized_pattern(pattern_random, what = "pp")
#'
#' \dontrun{
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_marks(pattern_random[[1]], marks_sub, n_random = 19, max_runs = 1000)
#' plot_randomized_pattern(marks_recon, method = "marks")
#' }
#'
#' @aliases plot_randomized_pattern
#' @rdname plot_randomized_pattern
#'
#' @export
plot_randomized_pattern <- function(pattern,
                                    what = "sf",
                                    method = "spatial",
                                    probs = c(0.025, 0.975),
                                    comp_fast = 1000,
                                    verbose = TRUE){

  # check if randomized and observed is present
  if(!all(c(paste0("randomized_", seq_len(length(pattern) - 1)), "observed") == names(pattern)) || is.null(names(pattern))) {
    stop("Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.",
         call. = FALSE)
  }

  if(what == "sf") {

    # check if number of points exceed comp_fast limit
    if(pattern$observed$n > comp_fast) {
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

    if(method == "spatial") {

      # loop through all input
      result_list <- lapply(seq_along(pattern), function(x){

        # calculate summary functions
        if(comp_fast) {

          gest_result <- spatstat::Gest(pattern[[x]],
                                        correction = "none",
                                        r = r)

          pcf_result <- shar::estimate_pcf_fast(pattern[[x]],
                                                correction = "none",
                                                method = "c",
                                                spar = 0.5,
                                                r = r)
        }

        else{
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
        if(verbose) {
          message("\r> Progress: ", x, "/", length(pattern), appendLF = FALSE)
        }

        return(summary_stats)
      })

      # get number of rows
      result_list_nrow <- vapply(X = result_list,
                                 FUN = nrow, FUN.VALUE = numeric(1))

      # combine results to one dataframe
      result_df <- do.call(rbind, result_list)

      # add pattern col
      result_df$pattern <- rep(names(pattern), times = result_list_nrow)

      # classify all observed and all randomized repetitions identical
      result_df$type[result_df$pattern != "observed"] <- "randomized"
      result_df$type[result_df$pattern == "observed"] <- "observed"

      result_df_summarised <- do.call(data.frame,
                                      stats::aggregate(x_r ~ summary_function + r + type,
                                                       data = result_df,
                                                       FUN = function(x){c(lo = stats::quantile(x, probs = probs[1]),
                                                                           hi = stats::quantile(x, probs = probs[2]),
                                                                           x_r = mean(x))}))
      # results Gest
      result_nndf <- result_df_summarised[result_df_summarised$summary_function == "G(r)", ]

      # results pcf
      result_pcf <- result_df_summarised[result_df_summarised$summary_function == "g(r)", ]

      graphics::par(mfrow = c(1, 2)) # two plots next to each other

      # plot Gest
      graphics::plot(NULL,
                     xlim = range(result_nndf[, 2]),
                     ylim = c(min(result_nndf[, 4]), max(result_nndf[, 5])),
                     main = "Nearest Neighbour Distance Function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "G(r)")

      graphics::polygon(x = c(result_nndf[, 2][result_nndf$type == "randomized"],
                              rev(result_nndf[, 2][result_nndf$type == "randomized"])),
                        y = c(result_nndf[, 4][result_nndf$type == "randomized"],
                              rev(result_nndf[, 5][result_nndf$type == "randomized"])),
                        col = 'grey80', border = NA)

      graphics::lines(x = result_nndf[, 2][result_nndf$type == "observed"],
                      y = result_nndf[, 6][result_nndf$type == "observed"])

      graphics::lines(x = result_nndf[, 2][result_nndf$type == "randomized"],
                      y = result_nndf[, 4][result_nndf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::lines(x = result_nndf[, 2][result_nndf$type == "randomized"],
                      y = result_nndf[, 5][result_nndf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::legend("bottomright",
                       legend = c("observed", "randomized"),
                       col = c("black", "red"), lty = c(1,2), inset = 0.025)

      # plot pcf
      graphics::plot(NULL,
                     xlim = range(result_pcf[, 2]),
                     ylim = c(min(result_pcf[, 4]), max(result_pcf[, 5])),
                     main = "Pair Correlation Function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "g(r)")

      graphics::polygon(x = c(result_pcf[, 2][result_pcf$type == "randomized"],
                              rev(result_pcf[, 2][result_pcf$type == "randomized"])),
                        y = c(result_pcf[, 4][result_pcf$type == "randomized"],
                              rev(result_pcf[, 5][result_pcf$type == "randomized"])),
                        col = 'grey80', border = NA)

      graphics::lines(x = result_pcf[, 2][result_pcf$type == "observed"],
                      y = result_pcf[, 6][result_pcf$type == "observed"])

      graphics::lines(x = result_pcf[, 2][result_pcf$type == "randomized"],
                      y = result_pcf[, 4][result_pcf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::lines(x = result_pcf[, 2][result_pcf$type == "randomized"],
                      y = result_pcf[, 5][result_pcf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::legend("topright",
                       legend = c("observed", "randomized"),
                       col = c("black", "red"), lty = c(1,2), inset = 0.025)

      graphics::par(mfrow = c(1, 1))
    }

    else if (method == "marks") {

      result_list <- lapply(seq_along(pattern), function(x){

        mark_corr <- as.data.frame(spatstat::markcorr(pattern[[x]],
                                                      correction = "Ripley",
                                                      r = r))

        # print progress
        if(verbose) {
          message("\r> Progress: ", x, "/", length(pattern), appendLF = FALSE)
        }

        return(mark_corr)
      })

      # get number of rows
      result_list_nrow <- vapply(X = result_list,
                                 FUN = nrow, FUN.VALUE = numeric(1))

      # combine results to one dataframe
      result_df <- do.call(rbind, result_list)

      # add pattern col
      result_df$pattern <- rep(names(pattern), times = result_list_nrow)

      # classify all observed and all randomized repetitions identical
      # classify all observed and all randomized repetitions identical
      result_df$type[result_df$pattern != "observed"] <- "randomized"
      result_df$type[result_df$pattern == "observed"] <- "observed"

      # calculate envelopes
      result_df_summarised <- do.call(data.frame,
                                      stats::aggregate(iso ~ r + type,
                                                       data = result_df,
                                                       FUN = function(x){c(lo = stats::quantile(x, probs = probs[1]),
                                                                           hi = stats::quantile(x, probs = probs[2]),
                                                                           x_r = mean(x))}))

      # plot results
      graphics::plot(NULL,
                     xlim = range(result_df_summarised[, 1]),
                     ylim = c(min(result_df_summarised[, 3]), max(result_df_summarised[, 4])),
                     main = "Mark correlation function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "kmm(r)")

      graphics::polygon(x = c(result_df_summarised[, 1][result_df_summarised$type == "randomized"],
                              rev(result_df_summarised[, 1][result_df_summarised$type == "randomized"])),
                        y = c(result_df_summarised[, 3][result_df_summarised$type == "randomized"],
                              rev(result_df_summarised[, 4][result_df_summarised$type == "randomized"])),
                        col = 'grey80', border = NA)

      graphics::lines(x = result_df_summarised[, 1][result_df_summarised$type == "observed"],
                      y = result_df_summarised[, 5][result_df_summarised$type == "observed"])

      graphics::lines(x = result_df_summarised[, 1][result_df_summarised$type == "randomized"],
                      y = result_df_summarised[, 3][result_df_summarised$type == "randomized"],
                      col = "red", lty = 2)

      graphics::lines(x = result_df_summarised[, 1][result_df_summarised$type == "randomized"],
                      y = result_df_summarised[, 4][result_df_summarised$type == "randomized"],
                      col = "red", lty = 2)

      graphics::legend("bottomright",
                       legend = c("observed", "randomized"),
                       col = c("black", "red"), lty = c(1,2), inset = 0.025)
    }

    else{
      stop("'method' must be either 'method = 'spatial'' or 'method = 'marks''",
           call. =FALSE)
    }
  }

  else if(what == "pp") {

    # get number of patterns
    number_patterns <- length(pattern)

    # check if at least 4 patterns are present, i.e. 3 randomized & observed
    if(number_patterns < 4) {
      stop("Please provide at least 3 randomizations and the observed pattern",
           call. = FALSE)
    }

    if(verbose) {
      message("> Plotting observed pattern and 4 randomized patterns only")
    }

    # sample 3 randomized patterns
    random_ids <- rcpp_sample(x = seq(from = 1, to = number_patterns - 1, by = 1),
                              n = 3)

    # get randomized and observed patterns
    pattern <- pattern[c(random_ids, number_patterns)]

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
  }

  else{
    stop("Please select either what = 'sf' or what = 'pp'.", call. = FALSE)
  }
}
