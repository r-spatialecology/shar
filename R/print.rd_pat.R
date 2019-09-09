#' print.rd_pat
#'
#' @description Print method for rd_pat object
#'
#' @param x Random patterns.
#' @param calc_energy If TRUE energy is calculated.
#' @param digits Number of decimal places (round).
#' @param weights Weights used to calculate energy. The first number refers to Gest(r),
#' the second number to pcf(r).
#' @param ... Arguments passed to cat
#'
#' @details
#' Printing method for random patterns created with \code{\link{reconstruct_pattern_homo}},
#' \code{\link{reconstruct_pattern_hetero}}, \code{\link{reconstruct_pattern_cluster}} or
#' \code{\link{fit_point_process}}.
#'
#' @seealso
#' \code{\link{reconstruct_pattern_homo}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}} \cr
#' \code{\link{fit_point_process}}
#'
#' @examples
#' pattern_random <- fit_point_process(species_a, n_random = 199)
#' print(pattern_random)
#'
#' \dontrun{
#' pattern_recon <- reconstruct_pattern_hetero(species_b, n_random = 19, max_runs = 1000)
#' print(pattern_recon)
#' }
#'
#' @aliases print.rd_pat
#' @rdname print.rd_pat

#' @export
print.rd_pat <- function(x,
                         calc_energy = TRUE,
                         digits = 4,
                         weights = c(0.5, 0.5),
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
                                                   weights = weights,
                                                   return_mean = TRUE,
                                                   verbose = FALSE),
                                  digits = digits),
                     error = function(e) NA)
  }

  # not calculate energy
  else {

    energy <- NA
  }

  # calculate mean iterations
  mean_iterations <- round(mean(unlist(x$iterations)),
                           digits = digits)

  # count stop criterions
  stop_criterion <- tryCatch(expr = table(do.call(c, x$stop_criterion), useNA = "ifany"),
                             error = function(e) NA)

  if (!any(is.na(stop_criterion))) {

    stop_criterion <- paste0(paste0(names(stop_criterion), "=", stop_criterion),
                             collapse = " ")
  }

  # print result
  cat(paste0("No. of pattern: ", number_patterns, "\n",
             "Mean energy: ", energy, "\n",
             "Method: ", x$method, "\n",
             "Observed pattern: ", includes_observed, "\n",
             "Iterations (mean): ", mean_iterations, "\n",
             "Stop criterion (no. of patterns): ", stop_criterion, "\n"), ...)
}
