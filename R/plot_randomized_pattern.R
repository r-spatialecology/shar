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

    if(method == "spatial") {

      # loop through all input
      result_list <- lapply(seq_along(pattern), function(x){

        # calculate summary functions
        if(comp_fast) {

          gest_result <- spatstat::Gest(pattern[[x]], correction = "none")

          pcf_result <- shar::estimate_pcf_fast(pattern[[x]],
                                                correction = "none",
                                                method = "c",
                                                spar = 0.5)
        }

        else{
          gest_result <- spatstat::Gest(pattern[[x]], correction = "han")

          pcf_result <- spatstat::pcf(pattern[[x]], divisor = "d", correction = "best")
        }

        gest_df <- as.data.frame(gest_result) # conver to df

        names(gest_df)[3] <- "x_r" # unique col names

        gest_df$summary_function <- "Nearest Neighbour Distance Function G(r)" # name of method

        pcf_df <- as.data.frame(pcf_result) # convert to df

        names(pcf_df)[3] <- "x_r" # unique col names

        pcf_df$summary_function <- "Pair Correlation Function g(r)" # name of method

        summary_stats <- dplyr::bind_rows(gest_df, pcf_df) # combine to one df

        # print progress
        if(verbose) {
          message("\r> Progress: ", x, "/", length(pattern), appendLF = FALSE)
        }

        return(summary_stats)
      })

      names(result_list) <- names(pattern) # add names of input to result list

      result_df <- dplyr::bind_rows(result_list, .id = "pattern") # combinte to one df

      # classify all observed and all randomized repetitions identical
      result_df <- dplyr::mutate(result_df, type = dplyr::case_when(pattern == "observed" ~ "observed",
                                                                    TRUE ~ "randomized"))

      result_df_grouped <- dplyr::group_by(result_df, summary_function, r, type) # group to calculate envelopes

      # calculate envelopes
      result_df_summarised <- dplyr::summarise(result_df_grouped,
                                               lo = stats::quantile(x_r, probs = probs[1]),
                                               hi = stats::quantile(x_r, probs = probs[2]),
                                               x_r = mean(x_r),
                                               theo = mean(theo))

      # results Gest
      result_nndf <- dplyr::filter(result_df_summarised,
                                   summary_function == "Nearest Neighbour Distance Function G(r)")

      # results pcf
      result_pcf <- dplyr::filter(result_df_summarised,
                                  summary_function == "Pair Correlation Function g(r)")

      graphics::par(mfrow = c(1, 2)) # two plots next to each other

      # plot Gest
      graphics::plot(x = result_nndf$r[result_nndf$type == "observed"],
                     y = result_nndf$x_r[result_nndf$type == "observed"], type = "l",
                     main = "Nearest Neighbour Distance Function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "G(r)")

      graphics::lines(x = result_nndf$r[result_nndf$type == "randomized"],
                      y = result_nndf$lo[result_nndf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::lines(x = result_nndf$r[result_nndf$type == "randomized"],
                      y = result_nndf$hi[result_nndf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::legend("bottomright",
                       legend = c("observed", "randomized"),
                       col = c("black", "red"), lty = c(1,2), inset = 0.025)

      # plot pcf
      graphics::plot(x = result_pcf$r[result_pcf$type == "observed"],
                     y = result_pcf$x_r[result_pcf$type == "observed"], type = "l",
                     main = "Pair Correlation Function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "g(r)")

      graphics::lines(x = result_pcf$r[result_pcf$type == "randomized"],
                      y = result_pcf$lo[result_pcf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::lines(x = result_pcf$r[result_pcf$type == "randomized"],
                      y = result_pcf$hi[result_pcf$type == "randomized"],
                      col = "red", lty = 2)

      graphics::legend("topright",
                       legend = c("observed", "randomized"),
                       col = c("black", "red"), lty = c(1,2), inset = 0.025)

      graphics::par(mfrow = c(1, 1))
    }

    else if (method == "marks") {

      result_list <- lapply(seq_along(pattern), function(x){

        mark_corr <- as.data.frame(spatstat::markcorr(pattern[[x]],
                                                      correction = "Ripley"))

        # print progress
        if(verbose) {
          message("\r> Progress: ", x, "/", length(pattern), appendLF = FALSE)
        }

        return(mark_corr)
      })

      names(result_list) <- names(pattern) # add names of input to result list

      result_df <- dplyr::bind_rows(result_list, .id = "pattern") # combinte to one df

      # classify all observed and all randomized repetitions identical
      result_df <- dplyr::mutate(result_df, type = dplyr::case_when(pattern == "observed" ~ "observed",
                                                                    TRUE ~ "randomized"))

      result_df_grouped <- dplyr::group_by(result_df, r, type) # group to calculate envelopes

      # calculate envelopes
      result_df_summarised <- dplyr::summarise(result_df_grouped,
                                               lo = stats::quantile(iso, probs = probs[1]),
                                               hi = stats::quantile(iso, probs = probs[2]),
                                               x_r = mean(iso),
                                               theo = mean(theo))

      graphics::plot(x = result_df_summarised$r[result_df_summarised$type == "observed"],
                     y = result_df_summarised$x_r[result_df_summarised$type == "observed"], type = "l",
                     main = "Mark correlation function",
                     xlab = paste0("r [",name_unit, "]"), ylab = "kmm(r)")

      graphics::lines(x = result_df_summarised$r[result_df_summarised$type == "randomized"],
                      y = result_df_summarised$lo[result_df_summarised$type == "randomized"],
                      col = "red", lty = 2)

      graphics::lines(x = result_df_summarised$r[result_df_summarised$type == "randomized"],
                      y = result_df_summarised$hi[result_df_summarised$type == "randomized"],
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

    message("> Plotting observed pattern and 4 randomized patterns only")

    # sample 3 randomized patterns
    random_ids <- sample(x = seq(from = 1, to = number_patterns - 1, by = 1),
                         size = 3)

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
