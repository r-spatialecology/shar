#' calculate_mean_energy
#'
#' @description Calculate mean energy
#'
#' @param pattern List with reconstructed patterns
#' @param comp_fast Logical if summary functions should be estimated in an computational
#' fast way (but without edge correction)
#'
#' @details
#' Calculate the mean energy between the observed pattern and all reconstructed patterns.
#'
#' @seealso
#' \code{\link{plot_randomized_pattern}} \cr
#' \code{\link{reconstruct_pattern}}
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pattern_recon <- SHAR::reconstruct_pattern(pattern_random, n_random = 9, max_runs = 1000)
#' calculate_mean_energy(pattern_recon)
#' }
#'
#' @aliases calculate_mean_energy
#' @rdname calculate_mean_energy
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859–871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.

#' @export
calculate_mean_energy <- function(pattern, comp_fast = FALSE){

  pattern_observed <- pattern[names(pattern) == "observed"]
  pattern_reconstructed <- pattern[names(pattern) != "observed"]

  if(isTRUE(comp_fast)) {
    gest_observed <- spatstat::Gest(X = pattern_observed[[1]], correction = "none")

    pcf_observed <- SHAR::estimate_pcf_fast(pattern = pattern_observed[[1]],
                                            correction = "none",
                                            method = "c",
                                            spar = 0.5)
  }

  else{
    gest_observed <- spatstat::Gest(X = pattern_observed[[1]], correction = "han")

    pcf_observed <- spatstat::pcf(X = pattern_observed[[1]],
                                  correction = "best", divisor = "d")
  }

  energy_all_recon <- sapply(pattern_reconstructed, function(current_pattern) {

    if(isTRUE(comp_fast)) {
      gest_reconstruction <- spatstat::Gest(X = current_pattern, correction = "none")

      pcf_reconstruction <- SHAR::estimate_pcf_fast(pattern = current_pattern,
                                                    correction = "none",
                                                    method = "c",
                                                    spar = 0.5)
    }

    else{
      gest_reconstruction <- spatstat::Gest(X = current_pattern, correction = "han")

      pcf_reconstruction <- spatstat::pcf(X = current_pattern,
                                          correction = "best", divisor = "d")
    }

    energy <- mean(abs(gest_observed[[3]] - gest_reconstruction[[3]]), na.rm = TRUE) +
      mean(abs(pcf_observed[[3]] - pcf_reconstruction[[3]]), na.rm = TRUE)

    return(energy)
  })

  result <- mean(energy_all_recon)

  return(result)
}

