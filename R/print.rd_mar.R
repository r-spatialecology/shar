#' print.rd_mar
#'
#' @description Print method for rd_mar object
#'
#' @param x Random patterns.
#' @param calc_energy If TRUE energy is calculated.
#' @param digits Number of decimal places (round).
#' @param ... Arguments passed to cat
#'
#' @details
#' Printing method for random patterns created with \code{\link{reconstruct_marks}}.
#'
#' @seealso
#' \code{\link{reconstruct_marks}}
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern(species_a, n_random = 1, max_runs = 1000,
#' simplify = TRUE, return_input = FALSE)
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_marks(pattern_recon, marks_sub, n_random = 19, max_runs = 1000)
#' print(marks_recon)
#' }
#'
#' @aliases print.rd_mar
#' @rdname print.rd_mar

#' @export
print.rd_mar <- function(x,
                         calc_energy = TRUE,
                         digits = 4,
                         ...) {

  # set length observed pattern to 0 and
  # return warning that energy can't be calculated
  if (!spatstat::is.ppp(x$observed)) {

    warning("Energy can not be calculated without observed pattern.",
            call. = FALSE)

    number_patterns_obs <- 0

    includes_observed <- "NA"
  }

  # observed pattern is present
  else {

    number_patterns_obs <- 1

    includes_observed <- "included"
  }

  # get number of randomized patterns plus observed pattern
  number_patterns <- length(x$randomized) + number_patterns_obs

  # calculate energy
  if (calc_energy) {

    # try to calculate energy
    energy <- tryCatch(expr = round(calculate_energy(x,
                                                     return_mean = TRUE,
                                                     verbose = FALSE),
                                    digits = digits),
                       error = function(e) "NA")
  }

  # not calculate energy
  else {

    energy <- NA
  }


  # calculate mean iterations
  mean_iterations <- round(mean(unlist(x$iterations)),
                           digits = digits)

  # print result
  cat(paste0("No. of pattern: ", number_patterns, "\n",
             "Mean energy: ", energy, "\n",
             "Method: ", x$method, "\n",
             "Observed pattern: ", includes_observed, "\n",
             "Iterations (mean): ", mean_iterations, "\n"),
        ...)
}
