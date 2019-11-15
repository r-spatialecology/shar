#' reconstruct_pattern_marks
#'
#' @description Pattern reconstruction of marks
#'
#' @param pattern ppp.
#' @param marked_pattern ppp (marked; see details).
#' @param n_random Number of randomizations.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param no_change Reconstrucction will stop if energy does not decrease for this number of iterations.
#' @param annealing Probability to keep relocated point even if energy did not decrease.
#' @param r_length Number of intervals from r = 0 to r = rmax the summary functions are evaluated.
#' @param return_input The original input data is returned as last list entry
#' @param simplify If n_random = 1 and return_input = FALSE only pattern will be returned.
#' @param verbose Print progress report.
#' @param plot Plot kmmr function during optimization.
#'
#' @details
#' The function randomizes the numeric marks of a point pattern using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). Therefore,
#' an unmarked as well as a marked pattern must be provided. The unmarked pattern must have
#' the spatial characteristics and the same observation window and number of points
#' as the marked one (see `reconstruct_pattern` or `fit_point_process`). Marks must be
#' numeric because the mark-correlation function is used as summary function. Two
#' randomly chosen marks are switch each iterations and changes only kept if the
#' deviation between the observed and the reconstructed pattern decreases.
#'
#' \code{spatstat} sets \code{r_length} to 513 by default. However, a lower value decreases
#' the computational time while increasing the "bumpiness" of the summary function.
#'
#' @seealso
#' \code{\link{fit_point_process}} \cr
#' \code{\link{reconstruct_pattern_homo}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern_homo(species_a, n_random = 1, max_runs = 1000,
#' simplify = TRUE, return_input = FALSE)
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_recon, marks_sub, n_random = 19, max_runs = 1000)
#' }
#'
#' @aliases reconstruct_pattern_marks
#' @rdname reconstruct_pattern_marks
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859-871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.
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
                                      return_input = TRUE,
                                      simplify = FALSE,
                                      verbose = TRUE,
                                      plot = FALSE){

  # check if n_random is >= 1
  if (!n_random >= 1) {

    stop("n_random must be >= 1.", call. = FALSE)
  }

  # check if pattern is marked
  if (spatstat::is.marked(pattern) || !spatstat::is.marked(marked_pattern)) {

    stop("'pattern' must be unmarked and 'marked_pattern' marked", call. = FALSE)
  }

  if (any(pattern$window$xrange != marked_pattern$window$xrange) ||
      any(pattern$window$yrange != marked_pattern$window$yrange) ||
      pattern$n != marked_pattern$n) {

    stop("'pattern' and 'pattern' must have same window and number of points",
         call. = FALSE)
  }

  # check if marks are numeric
  if (class(marked_pattern$marks) != "numeric") {

    stop("marks must be 'numeric'", call. = FALSE)
  }

  # set names of randomization randomized_1 ... randomized_n
  names_randomization <- paste0("randomized_", seq_len(n_random))

  # create empty lists for results
  energy_list <- vector("list", length = n_random)
  iterations_list <- vector("list", length = n_random)
  stop_criterion <- as.list(rep("max_runs", times = n_random))
  result_list <- vector("list", length = n_random)

  # set names
  names(energy_list) <- names_randomization
  names(result_list) <- names_randomization
  names(iterations_list) <- names_randomization
  names(stop_criterion) <- names_randomization

  # calculate r
  r <- seq(from = 0,
           to = spatstat::rmax.rule(W = pattern$window,
                                    lambda = spatstat::intensity.ppp(pattern)),
           length.out = r_length)

  # create random pattern
  simulated <- pattern

  # assign shuffled marks to pattern
  spatstat::marks(simulated) <- rcpp_sample(x = marked_pattern$marks, n = marked_pattern$n)

  # calculate summary functions
  kmmr_observed <- spatstat::markcorr(marked_pattern,
                                      correction = "Ripley",
                                      r = r)

  kmmr_simulated <- spatstat::markcorr(simulated,
                                       correction = "Ripley",
                                       r = r)

  # energy before reconstruction
  energy <- mean(abs(kmmr_observed[[3]] - kmmr_simulated[[3]]), na.rm = TRUE)

  # create n_random recondstructed patterns
  for (current_pattern in seq_len(n_random)) {

    # current simulated
    simulated_current <- simulated
    energy_current <- energy

    # counter of iterations
    iterations <- 0

    # counter if energy changed
    energy_counter <- 0

    # df for energy
    energy_df <- data.frame(i = seq(from = 1, to = max_runs, by = 1),
                            energy = NA)

    # get two random points to switch marks
    rp_a <- rcpp_sample(x = seq_len(simulated_current$n), n = max_runs, replace = TRUE)

    rp_b <- rcpp_sample(x = seq_len(simulated_current$n), n = max_runs, replace = TRUE)

    # create random number for annealing prob
    if (annealing != 0) {

      random_annealing <- stats::runif(n = max_runs, min = 0, max = 1)
    }

    else {

      random_annealing <- rep(0, max_runs)
    }

    # pattern reconstruction algorithm (optimaztion of energy) - not longer than max_runs
    for (i in seq_len(max_runs)) {

      relocated <- simulated_current # data for relocation

      # current random points
      rp_a_current <- rp_a[[i]]

      rp_b_current <- rp_b[[i]]

      # get marks of the two random points
      mark_a <- relocated$marks[[rp_a_current]]

      mark_b <- relocated$marks[[rp_b_current]]

      # switch the marks of the two points
      relocated$marks[[rp_a_current]] <- mark_b

      relocated$marks[[rp_b_current]] <- mark_a

      # calculate summary functions after relocation
      kmmr_relocated <- spatstat::markcorr(relocated,
                                           correction = "Ripley",
                                           r = r)

      # energy after relocation
      energy_relocated <- mean(abs(kmmr_observed[[3]] - kmmr_relocated[[3]]), na.rm = TRUE)

      # lower energy after relocation
      if (energy_relocated < energy_current || random_annealing[i] < annealing) {

        # keep relocated pattern
        simulated_current <- relocated

        # keep energy_relocated as energy
        energy_current <- energy_relocated

        # plot observed vs reconstructed
        if (plot) {

          # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion
          Sys.sleep(0.01)

          graphics::plot(x = kmmr_observed[[1]], y = kmmr_observed[[3]],
                         type = "l", col = "black",
                         xlab = "r", ylab = "kmm(r)")

          graphics::lines(x = kmmr_relocated[[1]], y = kmmr_relocated[[3]], col = "red")

          graphics::legend("topright",
                           legend = c("observed", "reconstructed"),
                           col = c("black", "red"),
                           lty = 1, inset = 0.025)
        }
      }

      # increase counter no change
      else {
        energy_counter <- energy_counter + 1
      }

      # count iterations
      iterations <- iterations + 1

      # save energy in data frame
      energy_df[iterations, 2] <- energy_current

      # print progress
      if (verbose) {

        if (!plot) {
          Sys.sleep(0.01)
        }

        message("\r> Progress: n_random: ", current_pattern, "/", n_random,
                " || max_runs: ", floor(i / max_runs * 100), "%",
                " || energy = ", round(energy_current, 5), "\t\t",
                appendLF = FALSE)
      }

      # exit loop if e threshold or no_change counter max is reached
      if (energy_current <= e_threshold || energy_counter > no_change) {

        # set stop criterion due to energy
        stop_criterion[[current_pattern]] <- "e_threshold/no_change"

        break
      }
    }

    if (plot) {
      grDevices::dev.off()
    }

    # remove NAs if stopped due to energy
    if (stop_criterion[[current_pattern]] == "e_threshold/no_change") {

      energy_df <- energy_df[1:iterations, ]
    }

    # save results in lists
    energy_list[[current_pattern]] <- energy_df
    iterations_list[[current_pattern]] <- iterations
    result_list[[current_pattern]] <- simulated_current
  }


  # combine to one list
  reconstruction <- list(randomized = result_list,
                         observed = marked_pattern,
                         method = "reconstruct_pattern_marks()",
                         energy_df = energy_list,
                         stop_criterion = stop_criterion,
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
      if (n_random > 1 && verbose) {

        warning("'simplify = TRUE' not possible for 'n_random > 1'.",
                call. = FALSE)
      }

      # only one random pattern is present that should be returend
      else if (n_random == 1) {
        reconstruction <- reconstruction$randomized[[1]]
      }
    }
  }

  # return input if return_input = TRUE
  else {

    # return warning if simply = TRUE because not possible if return_input = TRUE (only verbose = TRUE)
    if (simplify && verbose) {

      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)
    }
  }

  # write result in new line if progress was printed
  if (verbose) {
    message("\r")
  }

  return(reconstruction)
}
