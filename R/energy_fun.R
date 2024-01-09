#' energy_fun
#'
#' @description Energy function
#'
#' @param f Result of the calc_moments_full function which represents
#' product-moment contribution of a point at coordinates x, y with marks,
#' for the whole new ponit pattern.
#' @param f0  Column sums of the weights of the brand correlation functions of
#' the new point pattern.
#' @param statistics Results of the compute_statistics function for the
#' new point pattern (calculation of optional spatial statistics).
#' @param fn Determination of the weightings of the mark correlation functions.
#' @param p Defines the initial state of the new ponit pattern.
#' @param p_ Reference point pattern.
#' @param Lp Distance measure for the calculation of the energy function
#' (Lp distance, 1 <= p <Inf).
#' @param w_statistics Vector of named weights for optional spatial statistics
#' from the \code{spatstat} package to be included in the energy calculation.This may
#' include Dk, K, Hs, pcf.
#'
#' @details
#' Defining the Energy_fun function to calculate the "energy" of the pattern
#' (where a lower energy indicates a better match).
#'
#' @return vector
#'
#' @keywords internal
#'
Energy_fun <- function(f, f0, statistics, f_, f0_, statistics_, fn, p, p_, Lp, w_statistics) {
    result <- c(
      f = sum(fn$w * rowMeans(abs(
        f / nrow(p) -
        f_ / nrow(p_)
      )^Lp)),
      f0 = sum(fn$w0 * abs(
        f0 / nrow(p) -
        f0_ / nrow(p_)
      )^Lp),
      if (length(w_statistics))
        sapply(seq_along(w_statistics), function(i) w_statistics[i] *
          mean(abs(statistics[[i]] - statistics_[[i]])^Lp, na.rm = TRUE),
          USE.NAMES=FALSE
        )
    )
    c(energy = sum(result), result)
}
