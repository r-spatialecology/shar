#' reconstruct_pattern
#'
#' @description Pattern reconstruction
#'
#' @param pattern ppp object with pattern.
#' @param method Character with specifying the method. Either \code{"homo"},
#' \code{"cluster"} or \code{"hetero"}.
#' @param n_random Integer with number of randomizations.
#' @param e_threshold Double with minimum energy to stop reconstruction.
#' @param max_runs Integer with maximum number of iterations if \code{e_threshold}
#' is not reached.
#' @param no_change Integer with number of iterations at which the reconstruction will
#' stop if the energy does not decrease.
#' @param annealing Double with probability to keep relocated point even if energy
#' did not decrease.
#' @param weights Vector with weights used to calculate energy.
#' The first number refers to Gest(r), the second number to pcf(r).
#' @param r_length Integer with number of intervals from \code{r=0} to \code{r=rmax} for which
#' the summary functions are evaluated.
#' @param r_max Double with maximum distance used during calculation of summary functions. If \code{NULL},
#' will be estimated from data.
#' @param stoyan Coefficient for Stoyan's bandwidth selection rule.
#' @param return_input Logical if the original input data is returned.
#' @param simplify Logical if only pattern will be returned if \code{n_random=1}
#' and \code{return_input=FALSE}.
#' @param verbose Logical if progress report is printed.
#' @param plot Logical if pcf(r) function is plotted and updated during optimization.
#'
#' @details
#' The functions randomizes the observed pattern by using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). The
#' algorithm shifts a point to a new location and keeps the change only, if the
#' deviation between the observed and the reconstructed pattern decreases.
#' The pair correlation function and the nearest neighbour distance function are
#' used to describe the patterns.
#'
#' The reconstruction can be stopped automatically if for n steps the energy does not
#' decrease. The number of steps can be controlled by \code{no_change} and is set to
#' \code{no_change = Inf} as default to never stop automatically.
#'
#' The weights must be 0 < sum(weights) <= 1. To weight both summary functions identical,
#' use \code{weights = c(1, 1)}.
#'
#' \code{spatstat} sets \code{r_length} to 513 by default. However, a lower value decreases
#' the computational time, while increasing the "bumpiness" of the summary function.
#'
#' The arguments \code{n_points} and \code{window} are used for \code{method="homo"} only.
#'
#' \subsection{method="homo":}{
#' The algorithm starts with a random pattern.
#' }
#'
#' \subsection{method="cluster":}{
#' The algorithm starts with a random but clustered pattern.
#' }
#'
#' \subsection{method="hetero":}{
#' The algorithm starts with a random but heterogeneous pattern.
#' }
#'
#' @seealso
#' \code{\link{calculate_energy}} \cr
#' \code{\link{reconstruct_pattern_marks}}
#'
#' @return rd_pat
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern(species_b, n_random = 19, max_runs = 1000)
#' }
#'
#' @aliases reconstruct_pattern
#' @rdname reconstruct_pattern
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
reconstruct_pattern <- function(pattern, method = "homo",
                                n_random = 1,
                                e_threshold = 0.01,
                                max_runs,
                                no_change = Inf,
                                annealing = 0.01,
                                weights = c(1, 1),
                                r_length = 255,
                                r_max = NULL,
                                stoyan = 0.15,
                                return_input = TRUE,
                                simplify = FALSE,
                                verbose = TRUE,
                                plot = FALSE) {

  # check if correct method is selected
  if (!method %in% c("homo", "hetero", "cluster")) stop("Method must be one of the following: 'homo', 'hetero', or 'cluster'.",
                                                        call. = FALSE)

  reconstruction <- reconstruct_algorithm(pattern = pattern, method = method, n_random = n_random,
                                          e_threshold = e_threshold, max_runs = max_runs,
                                          no_change = no_change, annealing = annealing,
                                          weights = weights, r_length = r_length, r_max = r_max,
                                          stoyan = stoyan, verbose = verbose, plot = plot)

  # set class of result
  class(reconstruction) <- "rd_pat"

  # remove input if return_input = FALSE
  if (!return_input) {

    # set observed to NA
    reconstruction$observed <- "NA"

    # check if output should be simplified
    if (simplify) {

      # not possible if more than one pattern is present
      if (n_random > 1) {

        warning("'simplify = TRUE' not possible for 'n_random > 1'.",
                call. = FALSE)

        # only one random pattern is present that should be returend
      } else if (n_random == 1) {

        reconstruction <- reconstruction$randomized[[1]]

      }
    }

    # return input if return_input = TRUE
  } else {

    # return warning if simply = TRUE because not possible if return_input = TRUE (only verbose = TRUE)
    if (simplify) {

      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)

    }
  }

  return(reconstruction)

}
