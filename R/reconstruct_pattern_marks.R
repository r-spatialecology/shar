#' reconstruct_pattern_marks
#'
#' @description Pattern reconstruction of marked pattern
#'
#' @param pattern ppp object with pattern.
#' @param marked_pattern ppp  object with marked pattern. See Details section for more information.
#' @param n_random Integer with number of randomizations.
#' @param e_threshold Double with minimum energy to stop reconstruction.
#' @param max_runs Integer with maximum number of iterations if \code{e_threshold}
#' is not reached.
#' @param no_change Integer with number of iterations at which the reconstruction will
#' stop if the energy does not decrease.
#' @param annealing Double with probability to keep relocated point even if energy
#' did not decrease.
#' @param r_length Integer with number of intervals from \code{r = 0} to \code{r = rmax} for which
#' the summary functions are evaluated.
#' @param r_max Double with maximum distance used during calculation of summary functions. If \code{NULL},
#' will be estimated from data.
#' @param return_input Logical if the original input data is returned.
#' @param simplify Logical if only pattern will be returned if \code{n_random = 1}
#' and \code{return_input = FALSE}.
#' @param verbose Logical if progress report is printed.
#' @param plot Logical if pcf(r) function is plotted and updated during optimization.
#' @details
#' The function randomizes the numeric marks of a point pattern using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). Therefore,
#' an unmarked as well as a marked pattern must be provided. The unmarked pattern must have
#' the spatial characteristics and the same observation window and number of points
#' as the marked one (see \code{reconstruct_pattern_*} or \code{\link{fit_point_process}}).
#' Marks must be numeric because the mark-correlation function is used as summary function.
#' Two randomly chosen marks are switch each iterations and changes only kept if the
#' deviation between the observed and the reconstructed pattern decreases.
#'
#' \code{spatstat} sets \code{r_length} to 513 by default. However, a lower value decreases
#' the computational time while increasing the "bumpiness" of the summary function.
#'
#' @seealso
#' \code{\link{fit_point_process}} \cr
#' \code{\link{reconstruct_pattern}}
#'
#' @return rd_mar
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern(species_a, n_random = 1, max_runs = 1000,
#' simplify = TRUE, return_input = FALSE)
#' marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_recon, marks_sub,
#' n_random = 19, max_runs = 1000)
#' }
#'
#' @aliases reconstruct_pattern_marks
#' @rdname reconstruct_pattern_marks
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
reconstruct_pattern_marks <- function(pattern,
                                      marked_pattern,
                                      n_random = 1,
                                      e_threshold = 0.01,
                                      max_runs = 10000,
                                      no_change = Inf,
                                      annealing = 0.01,
                                      r_length = 250,
                                      r_max = NULL,
                                      return_input = TRUE,
                                      simplify = FALSE,
                                      verbose = TRUE,
                                      plot = FALSE){

  # check if n_random is >= 1
  if (!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  # check if pattern is marked
  if (spatstat.geom::is.marked(pattern) || !spatstat.geom::is.marked(marked_pattern)) {
    stop("'pattern' must be unmarked and 'marked_pattern' marked", call. = FALSE)
  }

  # check if marks are numeric
  if (!inherits(x = marked_pattern$marks, what = "numeric")) {
    stop("marks must be 'numeric'", call. = FALSE)
  }

  if (pattern$n == 0 ||  marked_pattern$n == 0){
    stop("At least one of the observed patterns contain no points.", call. = FALSE)
  }

  # calculate r from data
  if (is.null(r_max)) {

    r <- seq(from = 0, to = spatstat.explore::rmax.rule(W = pattern$window, lambda = spatstat.geom::intensity.ppp(pattern)),
             length.out = r_length)

  # use provided r_max
  } else {

    r <- seq(from = 0, to = r_max, length.out = r_length)

  }

  # set names of randomization randomized_1 ... randomized_n
  names_randomization <- paste0("randomized_", seq_len(n_random))

  # create empty lists for results
  energy_list <- vector("list", length = n_random)
  iterations_list <- vector("list", length = n_random)
  stop_criterion_list <- as.list(rep("max_runs", times = n_random))
  result_list <- vector("list", length = n_random)

  # set names
  names(energy_list) <- names_randomization
  names(iterations_list) <- names_randomization
  names(stop_criterion_list) <- names_randomization
  names(result_list) <- names_randomization

  # calculate summary functions
  kmmr_observed <- spatstat.explore::markcorr(marked_pattern, correction = "none", r = r)

  # create n_random recondstructed patterns
  for (i in seq_len(n_random)) {

    # create random pattern
    simulated <- pattern

    # assign shuffled marks to pattern
    spatstat.geom::marks(simulated) <- sample(x = marked_pattern$marks, size = simulated$n,
                                              replace = TRUE)

    energy <- Inf

    # counter if energy changed
    energy_counter <- 0

    # df for energy
    energy_df <- data.frame(i = seq(from = 1, to = max_runs, by = 1),
                            energy = NA)

    # create random number for annealing prob
    if (annealing != 0) {

      random_annealing <- stats::runif(n = max_runs, min = 0, max = 1)

    } else {

      random_annealing <- rep(0, max_runs)

    }

    # get two random points to switch marks
    rp_a <- sample(x = seq_len(simulated$n), size = max_runs, replace = TRUE)

    rp_b <- sample(x = seq_len(simulated$n), size = max_runs, replace = TRUE)

    # pattern reconstruction algorithm (optimaztion of energy) - not longer than max_runs
    for (j in seq_len(max_runs)) {

      relocated <- simulated # data for relocation

      # current random points
      a_current <- rp_a[[j]]

      b_current <- rp_b[[j]]

      # get marks of the two random points
      mark_a <- relocated$marks[[a_current]]

      mark_b <- relocated$marks[[b_current]]

      # switch the marks of the two points
      relocated$marks[[a_current]] <- mark_b

      relocated$marks[[b_current]] <- mark_a

      # calculate summary functions after relocation
      kmmr_relocated <- spatstat.explore::markcorr(relocated, correction = "none",
                                                   r = r)

      # energy after relocation
      energy_relocated <- mean(abs(kmmr_observed[[3]] - kmmr_relocated[[3]]), na.rm = TRUE)

      # lower energy after relocation
      if (energy_relocated < energy || random_annealing[i] < annealing) {

        # keep relocated pattern
        simulated <- relocated

        # keep energy_relocated as energy
        energy <- energy_relocated

        # plot observed vs reconstructed
        if (plot) {

          # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion
          Sys.sleep(0.01)

          graphics::plot(x = kmmr_observed[[1]], y = kmmr_observed[[3]],
                         type = "l", col = "black", xlab = "r", ylab = "kmm(r)")

          graphics::abline(h = 1, lty = 2, col = "grey")

          graphics::lines(x = kmmr_relocated[[1]], y = kmmr_relocated[[3]], col = "red")

          graphics::legend("topright", legend = c("observed", "reconstructed"),
                           col = c("black", "red"), lty = 1, inset = 0.025)

        }

      # increase counter no change
      } else {

        energy_counter <- energy_counter + 1

      }

      # save energy in data frame
      energy_df[j, 2] <- energy

      # print progress
      if (verbose) {

        message("\r> Progress: n_random: ", i, "/", n_random,
                " || max_runs: ", floor(j / max_runs * 100), "%",
                " || energy = ", round(energy, 5), "\t\t",
                appendLF = FALSE)

      }

      # exit loop if e threshold or no_change counter max is reached
      if (energy <= e_threshold || energy_counter > no_change) {

        # set stop criterion due to energy
        stop_criterion_list[[i]] <- ifelse(test = energy <= e_threshold,
                                           yes = "e_threshold", no = "no_change")

        break

      }
    }

    if (plot) {

      grDevices::dev.off()

    }

    # remove NAs if stopped due to energy
    if (stop_criterion_list[[i]] %in% c("e_threshold", "no_change")) {

      energy_df <- energy_df[1:j, ]

    }

    # save results in lists
    energy_list[[i]] <- energy_df
    iterations_list[[i]] <- j
    result_list[[i]] <- simulated

  }

  # write result in new line if progress was printed
  if (verbose) {

    message("\r")

  }

  # combine to one list
  reconstruction <- list(randomized = result_list, observed = marked_pattern,
                         method = "marks",
                         energy_df = energy_list, stop_criterion = stop_criterion_list,
                         iterations = iterations_list)

  # set class of returning object
  class(reconstruction) <- "rd_mar"

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

      }

      # only one random pattern is present that should be returend
      else if (n_random == 1) {

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
