#' calculate_energy
#'
#' @description Calculate mean energy
#'
#' @param pattern List with reconstructed patterns.
#' @param weights Weights used to calculate energy. The first number refers to Gest(r), the second number to pcf(r).
#' @param return_mean Return the mean energy.
#' @param comp_fast If pattern contains more points than threshold, summary functions are estimated in a computational fast way.
#' @param verbose Print progress report.
#'
#' @details
#' The function calculates the mean energy (or deviation) between the observed
#' pattern and all reconstructed patterns (for more information see Tscheschel &
#' Stoyan (2006) or Wiegand & Moloney (2014)). The pair correlation function and the
#' nearest neighbour distance function are used to describe the patterns. For large
#' patterns `comp_fast = TRUE` decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' @seealso
#' \code{\link{plot_energy}} \cr
#' \code{\link{reconstruct_pattern_homo}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @return numeric
#'
#' @examples
#' pattern_random <- fit_point_process(species_a, n_random = 19)
#' calculate_energy(pattern_random)
#' calculate_energy(pattern_random, return_mean = TRUE)
#'
#' \dontrun{
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_random$randomized[[1]], marks_sub,
#' n_random = 19, max_runs = 1000)
#' calculate_energy(marks_recon, return_mean = FALSE)
#' }
#'
#' @aliases calculate_energy
#' @rdname calculate_energy
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859-871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.

#' @export
calculate_energy <- function(pattern,
                             weights = c(0.5, 0.5),
                             return_mean = FALSE,
                             comp_fast = 1000,
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

  # extract observed pattern
  pattern_observed <- pattern$observed

  # extract randomized patterns
  pattern_randomized <- pattern$randomized

  # calculate r sequence
  r <- seq(from = 0,
           to = spatstat::rmax.rule(W = pattern_observed$window,
                                    lambda = spatstat::intensity.ppp(pattern_observed)),
           length.out = 250)

  if (class(pattern) == "rd_pat") {

    # get energy from df
    if (is.list(pattern$energy_df)) {

      result <- vapply(pattern$energy_df, FUN = function(x) utils::tail(x, n = 1)[[2]],
                       FUN.VALUE = numeric(1))

    }

    else {

      # check if weights make sense
      if (sum(weights) > 1 || sum(weights) == 0) {
        stop("The sum of 'weights' must be 0 < sum(weights) <= 1.", call. = FALSE)
      }

      # check if number of points exceed comp_fast limit
      if (pattern_observed$n > comp_fast) {
        comp_fast <- TRUE
      }

      else {
        comp_fast <- FALSE
      }

      # calculate summary functions for observed pattern
      if (comp_fast) {

        gest_observed <- spatstat::Gest(X = pattern_observed,
                                        correction = "none",
                                        r = r)

        pcf_observed <- shar::estimate_pcf_fast(pattern = pattern_observed,
                                                correction = "none",
                                                method = "c",
                                                spar = 0.5,
                                                r = r)
      }

      else{

        gest_observed <- spatstat::Gest(X = pattern_observed,
                                        correction = "han",
                                        r = r)

        pcf_observed <- spatstat::pcf(X = pattern_observed,
                                      correction = "best",
                                      divisor = "d",
                                      r = r)
      }

      # loop through all reconstructed patterns
      result <- vapply(seq_along(pattern_randomized), function(x) {

        # fast computation of summary stats
        if (comp_fast) {

          gest_reconstruction <- spatstat::Gest(X = pattern_randomized[[x]],
                                                correction = "none",
                                                r = r)

          pcf_reconstruction <- shar::estimate_pcf_fast(pattern = pattern_randomized[[x]],
                                                        correction = "none",
                                                        method = "c",
                                                        spar = 0.5,
                                                        r = r)
        }

        # normal computation of summary stats
        else{

          gest_reconstruction <- spatstat::Gest(X = pattern_randomized[[x]],
                                                correction = "han",
                                                r = r)

          pcf_reconstruction <- spatstat::pcf(X = pattern_randomized[[x]],
                                              correction = "best",
                                              divisor = "d",
                                              r = r)
        }

        # difference between observed and reconstructed pattern
        energy <- (mean(abs(gest_observed[[3]] - gest_reconstruction[[3]]), na.rm = TRUE) * weights[[1]]) +
          (mean(abs(pcf_observed[[3]] - pcf_reconstruction[[3]]), na.rm = TRUE) * weights[[2]])

        # print progress
        if (verbose) {
          message("\r> Progress: ", x, "/", length(pattern_randomized), "\t\t",
                  appendLF = FALSE)
        }

        return(energy)

      }, FUN.VALUE = numeric(1))
    }

    # set names
    names(result) <- paste0("randomized_", seq_along(result))
  }

  else if (class(pattern) == "rd_mar") {

    # get energy from df
    if (is.list(pattern$energy_df)) {

      result <- vapply(pattern$energy_df, FUN = function(x) utils::tail(x, n = 1)[[2]],
                       FUN.VALUE = numeric(1))
    }

    else {

      # calculate summary functions
      kmmr_observed <- spatstat::markcorr(pattern_observed,
                                          correction = "Ripley",
                                          r = r)

      result <- vapply(seq_along(pattern_randomized), function(x) {

        # calculate summary functions
        kmmr_reconstruction <- spatstat::markcorr(pattern_randomized[[x]],
                                                  correction = "Ripley",
                                                  r = r)

        # difference between observed and reconstructed pattern
        energy <- mean(abs(kmmr_observed[[3]] - kmmr_reconstruction[[3]]), na.rm = TRUE)

        # print progress
        if (verbose) {
          message("\r> Progress: ", x, "/", length(pattern_randomized), "\t\t",
                  appendLF = FALSE)
        }

        return(energy)

      }, FUN.VALUE = numeric(1))
    }

    # set names
    names(result) <- paste0("randomized_", seq_along(result))
  }

  # return mean for all reconstructed patterns
  if (return_mean) {
    result <- mean(result)
  }

  # write result in new line if progress was printed
  if (verbose) {
    message("\r")
  }

  return(result)
}
