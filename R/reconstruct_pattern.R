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
#' @param comp_fast Integer with threshold at which summary functions are estimated
#' in a computational fast way.
#' @param n_points Integer with number of points to be simulated.
#' @param window owin object with window of simulated pattern.
#' @param weights Vector with weights used to calculate energy.
#' The first number refers to Gest(r), the second number to pcf(r).
#' @param r_length Integer with number of intervals from \code{r=0} to \code{r=rmax} for which
#' the summary functions are evaluated.
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
#' For large patterns (\code{n > comp_fast}) the pair correlation function can be estimated
#' from Ripley's K-function without edge correction. This decreases the computational
#' time. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' The reconstruction can be stopped automatically if for n steps the energy does not
#' decrease. The number of steps can be controlled by \code{no_change} and is set to
#' \code{no_change = Inf} as default to never stop automatically.
#'
#' The weights must be 0 < sum(weights) <= 1. To weight both summary functions identical,
#' use \code{weights = c(0.5, 0.5)}.
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
#' \code{\link{plot_randomized_pattern}} \cr
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
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859-871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.
#'
#' @export
reconstruct_pattern <- function(pattern, method = "homo",
                                n_random = 1,
                                e_threshold = 0.01,
                                max_runs = 1000,
                                no_change = Inf,
                                annealing = 0.01,
                                comp_fast = 1000,
                                n_points = NULL,
                                window = NULL,
                                weights = c(0.5, 0.5),
                                r_length = 250,
                                return_input = TRUE,
                                simplify = FALSE,
                                verbose = TRUE,
                                plot = FALSE) {

  if (method == "homo") {

    reconstruction <- reconstruct_pattern_homo(pattern = pattern, n_random = n_random,
                                               e_threshold = e_threshold, max_runs = max_runs,
                                               no_change = no_change, annealing = annealing,
                                               comp_fast = comp_fast, n_points = n_points,
                                               window = window, weights = weights,
                                               r_length = r_length, return_input = return_input,
                                               simplify = simplify, verbose = verbose,
                                               plot = plot)



  } else if (method == "cluster") {

    if (verbose && !is.null(n_points) && !is.null(window)) {

      warning("'n_points' and 'window' are not used for method='cluster'.", call. = FALSE)

    }

    reconstruction <- reconstruct_pattern_cluster(pattern = pattern, n_random = n_random,
                                                  e_threshold = e_threshold, max_runs = max_runs,
                                                  no_change = no_change, annealing = annealing,
                                                  comp_fast = comp_fast, weights = weights,
                                                  r_length = r_length, return_input = return_input,
                                                  simplify = simplify, verbose = verbose,
                                                  plot = plot)

  } else if (method == "hetero") {

    if (verbose && !is.null(n_points) && !is.null(window)) {

      warning("'n_points' and 'window' are not used for method='hetero'.", call. = FALSE)

    }

    reconstruction <- reconstruct_pattern_hetero(pattern = pattern, n_random = n_random,
                                                 e_threshold = e_threshold, max_runs = max_runs,
                                                 no_change = no_change, annealing = annealing,
                                                 comp_fast = comp_fast, weights = weights,
                                                 r_length = r_length, return_input = return_input,
                                                 simplify = simplify, verbose = verbose,
                                                 plot = plot)

  } else {

    stop("Method must be one of the following: 'homo', 'cluster', 'hetero', or 'marks'.",
         call. = FALSE)

  }

  return(reconstruction)
}
