#' print.rd_mar
#'
#' @description Print method for rd_mar object
#'
#' @param x Random patterns.
#' @param digits Number of decimal places (round).
#' @param ... Arguments passed to cat
#'
#' @details
#' Printing method for random patterns created with \code{\link{reconstruct_pattern_marks}}.
#'
#' @seealso
#' \code{\link{reconstruct_pattern_marks}}
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern_homo(species_a, n_random = 1, max_runs = 1000,
#' simplify = TRUE, return_input = FALSE)
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_recon, marks_sub, n_random = 19, max_runs = 1000)
#' print(marks_recon)
#' }
#'
#' @aliases print.rd_mar
#' @rdname print.rd_mar

#' @export
print.rd_mar <- function(x,
                         digits = 4,
                         ...) {

  # set length observed pattern to 0 and
  # return warning that energy can't be calculated
  if (!spatstat::is.ppp(x$observed)) {

    number_patterns_obs <- 0

    includes_observed <- "NA"
  }

  # observed pattern is present
  else {

    number_patterns_obs <- 1

    includes_observed <- "included"
  }

  # get extent of window
  extent_window <- paste0(c(x$randomized[[1]]$window$xrange,
                            x$randomized[[1]]$window$yrange), collapse = " ")


  # get number of randomized patterns plus observed pattern
  number_patterns <- length(x$randomized) + number_patterns_obs

  # calculate mean iterations
  mean_iterations <- round(mean(unlist(x$iterations)),
                           digits = digits)

  # count stop criterions
  stop_criterion <- tryCatch(expr = table(do.call(c, x$stop_criterion), useNA = "ifany"),
                             error = function(e) NA)

  if (!any(is.na(stop_criterion))) {

    stop_criterion <- paste0(paste0(names(stop_criterion), ":", stop_criterion),
                             collapse = " ")
  }

  # check if eneergy_df is available
  if (is.list(x$energy_df)) {

    mean_energy <- round(mean(vapply(x$energy_df, function(x) {

      utils::tail(x, n = 1)[, 2]
    }, FUN.VALUE = numeric(1))), digits = digits)
  }

  else {
    mean_energy <- "NA"
  }

  # print result
  cat(paste0("No. of pattern: ", number_patterns, "\n",
             "Method: ", x$method, "\n",
             "Observed pattern: ", includes_observed, "\n",
             "Iterations (mean): ", mean_iterations, "\n",
             "Energy (mean): ", mean_energy, "\n",
             "Stop criterion (no. of patterns): ", stop_criterion, "\n",
             "Extent: ", extent_window, " (xmin, xmax, ymin, ymax) \n"), ...)
}
