#' calculate_energy
#'
#' @description Calculate mean energy
#'
#' @param pattern List with reconstructed patterns.
#' @param weights Vector with weights used to calculate energy.
#' The first number refers to Gest(r), the second number to pcf(r).
#' @param return_mean Logical if the mean energy is returned.
#' @param verbose Logical if progress report is printed.
#'
#' @details
#' The function calculates the mean energy (or deviation) between the observed
#' pattern and all reconstructed patterns (for more information see Tscheschel &
#' Stoyan (2006) or Wiegand & Moloney (2014)). The pair correlation function and the
#' nearest neighbour distance function are used to describe the patterns.
#'
#' @seealso
#' \code{\link{plot_energy}},
#' \code{\link{reconstruct_pattern}},
#' \code{\link{fit_point_process}}
#'
#' @return vector
#'
#' @examples
#' pattern_random <- fit_point_process(species_a, n_random = 19)
#' calculate_energy(pattern_random)
#' calculate_energy(pattern_random, return_mean = TRUE)
#'
#' \dontrun{
#' marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_random$randomized[[1]], marks_sub,
#' n_random = 19, max_runs = 1000)
#' calculate_energy(marks_recon, return_mean = FALSE)
#' }
#'
#' @references
#' Kirkpatrick, S., Gelatt, C.D.Jr., Vecchi, M.P., 1983. Optimization by simulated
#' annealing. Science 220, 671–680. <https://doi.org/10.1126/science.220.4598.671>
#'
#' Tscheschel, A., Stoyan, D., 2006. Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis 51, 859–871.
#' <https://doi.org/10.1016/j.csda.2005.09.007>
#'
#' Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern analysis in
#' ecology. Chapman and Hall/CRC Press, Boca Raton. ISBN 978-1-4200-8254-8
#'
#' @export
calculate_energy <- function(pattern,
                             weights = c(1, 1),
                             return_mean = FALSE,
                             verbose = TRUE){

  # check if class is correct
  if (!inherits(x = pattern, what = c("rd_pat", "rd_mar"))) {

    stop("Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
         call. = FALSE)

  }

  # check if observed pattern is present
  if (!spatstat.geom::is.ppp(pattern$observed)) {

    stop("Input must include 'observed' pattern.", call. = FALSE)

  }

  # extract observed pattern
  pattern_observed <- pattern$observed

  # extract randomized patterns
  pattern_randomized <- pattern$randomized

  # calculate r sequence
  r <- seq(from = 0,
           to = spatstat.explore::rmax.rule(W = pattern_observed$window,
                                            lambda = spatstat.geom::intensity.ppp(pattern_observed)),
           length.out = 250)

  if (inherits(x = pattern, what = "rd_pat")) {

    # get energy from df
    if (is.list(pattern$energy_df)) {

      result <- vapply(pattern$energy_df, FUN = function(x) utils::tail(x, n = 1)[[2]],
                       FUN.VALUE = numeric(1))

    } else {

      # calculate summary functions for observed pattern
      gest_observed <- spatstat.explore::Gest(X = pattern_observed,
                                              correction = "none", r = r)

      pcf_observed <- spatstat.explore::pcf.ppp(X = pattern_observed, correction = "none", divisor = "d", r = r)

      # loop through all reconstructed patterns
      result <- vapply(seq_along(pattern_randomized), function(x) {

        gest_reconstruction <- spatstat.explore::Gest(X = pattern_randomized[[x]],
                                                      correction = "none", r = r)

          pcf_reconstruction <- spatstat.explore::pcf.ppp(X = pattern_randomized[[x]],
                                                          correction = "none", divisor = "d",
                                                          r = r)

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

  } else if (inherits(x = pattern, what = "rd_mar")) {

    # get energy from df
    if (is.list(pattern$energy_df)) {

      result <- vapply(pattern$energy_df, FUN = function(x) utils::tail(x, n = 1)[[2]],
                       FUN.VALUE = numeric(1))

    } else {

      # calculate summary functions
      kmmr_observed <- spatstat.explore::markcorr(pattern_observed, correction = "Ripley",
                                               r = r)

      result <- vapply(seq_along(pattern_randomized), function(x) {

        # calculate summary functions
        kmmr_reconstruction <- spatstat.explore::markcorr(pattern_randomized[[x]],
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
