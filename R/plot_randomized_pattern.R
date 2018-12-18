#' plot_randomized_pattern
#'
#' @description Plot randomized pattern
#'
#' @param pattern List with reconstructed patterns.
#' @param probs Quantiles of randomized data used for envelope construction.
#' @param comp_fast Should summary functions be estimated in an computational fast way.
#'
#' @details
#' The function plots the pair correlation function and the nearest neighbour function
#' the observed pattern and the reconstructed patterns (as "simulation envelopes".).
#' For large patterns `comp_fast = TRUE` decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' @return ggplot
#'
#' @examples
#' pattern_recon <- fit_point_process(species_b, n_random = 39, process = "cluster")
#' plot_randomized_pattern(pattern_recon)
#'
#' @aliases plot_randomized_pattern
#' @rdname plot_randomized_pattern
#'
#' @export
plot_randomized_pattern <- function(pattern,
                                    probs = c(0.025, 0.975),
                                    comp_fast = FALSE){

  # check if randomized and observed is present
  if(!all(c(paste0("randomized_", seq_len(length(pattern) - 1)), "observed") == names(pattern)) || is.null(names(pattern))) {
    stop("Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.",
         call. = FALSE)
  }

  name_unit <- spatstat::unitname(pattern$observed)[[1]] # unit name for labels

  # loop through all input
  result_list <- lapply(pattern, function(current_pattern){

    # calculate summary functions
    if(comp_fast) {

      gest_result <- spatstat::Gest(current_pattern, correction = "none")

      pcf_result <- SHAR::estimate_pcf_fast(current_pattern,
                                            correction = "none",
                                            method = "c",
                                            spar = 0.5)
    }

    else{
      gest_result <- spatstat::Gest(current_pattern, correction = "han")

      pcf_result <- spatstat::pcf(current_pattern, divisor = "d", correction = "best")
    }

    gest_df <- as.data.frame(gest_result) # conver to df

    names(gest_df)[3] <- "x_r" # unique col names

    gest_df$summary_function <- "Nearest Neighbour Distance Function G(r)" # name of method

    pcf_df <- as.data.frame(pcf_result) # convert to df

    names(pcf_df)[3] <- "x_r" # unique col names

    pcf_df$summary_function <- "Pair Correlation Function g(r)" # name of method

    dplyr::bind_rows(gest_df, pcf_df) # combine to one df
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

