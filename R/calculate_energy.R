#' calculate_energy
#'
#' @description Calculate mean energy
#'
#' @param pattern List with reconstructed patterns.
#' @param return_mean Return the mean energy
#' @param comp_fast If pattern contains more points than threshol, summary functions are estimated in a computational fast way.
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
#' \code{\link{reconstruct_pattern}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @return numeric
#'
#' @examples
#' pattern_random <- fit_point_process(species_a, n_random = 39)
#' calculate_energy(pattern_random)
#' calculate_energy(pattern_random, return_mean = TRUE)
#'
#' @aliases calculate_energy
#' @rdname calculate_energy
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859–871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.

#' @export
calculate_energy <- function(pattern, return_mean = FALSE, comp_fast = 1000, verbose = TRUE){

  # check if randomized and observed is present
  if(!all(c(paste0("randomized_", seq_len(length(pattern) - 1)), "observed") == names(pattern)) || is.null(names(pattern))) {
    stop("Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.",
         call. = FALSE)
  }

  # check if number of points exceed comp_fast limit
  if(pattern$observed$n > comp_fast) {
    comp_fast <- TRUE
  }

  else {
    comp_fast <- FALSE
  }

  pattern_observed <- pattern[names(pattern) == "observed"] # extract observed pattern

  pattern_reconstructed <- pattern[names(pattern) != "observed"] # extract randomized patterns

  # calculate summary functions for observed pattern
  if(comp_fast) {

    gest_observed <- spatstat::Gest(X = pattern_observed[[1]], correction = "none")

    pcf_observed <- shar::estimate_pcf_fast(pattern = pattern_observed[[1]],
                                            correction = "none",
                                            method = "c",
                                            spar = 0.5)
  }

  else{

    gest_observed <- spatstat::Gest(X = pattern_observed[[1]], correction = "han")

    pcf_observed <- spatstat::pcf(X = pattern_observed[[1]],
                                  correction = "best", divisor = "d")
  }

  # loop through all reconstructed patterns
  result <- vapply(seq_along(pattern_reconstructed), function(x) {

    # fast computation of summary stats
    if(comp_fast) {

      gest_reconstruction <- spatstat::Gest(X = pattern_reconstructed[[x]], correction = "none")

      pcf_reconstruction <- shar::estimate_pcf_fast(pattern = pattern_reconstructed[[x]],
                                                    correction = "none",
                                                    method = "c",
                                                    spar = 0.5)
    }

    # normal computation of summary stats
    else{

      gest_reconstruction <- spatstat::Gest(X = pattern_reconstructed[[x]], correction = "han")

      pcf_reconstruction <- spatstat::pcf(X = pattern_reconstructed[[x]],
                                          correction = "best", divisor = "d")
    }

    # difference between observed and reconstructed pattern
    energy <- mean(abs(gest_observed[[3]] - gest_reconstruction[[3]]), na.rm = TRUE) +
      mean(abs(pcf_observed[[3]] - pcf_reconstruction[[3]]), na.rm = TRUE)

    # print progress
    if(verbose) {
      message("\r> Progress: ", x, "/", length(pattern_reconstructed), appendLF = FALSE)
    }

    return(energy)

  }, FUN.VALUE = numeric(1))

  # return mean for all reconstructed patterns
  if(return_mean) {
    result <- mean(result)
  }

  # write result in new line if progress was printed
  if(verbose) {
    message("\r")
  }

  return(result)
}
