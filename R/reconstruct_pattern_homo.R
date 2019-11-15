#' reconstruct_pattern_homo
#'
#' @description Pattern reconstruction
#'
#' @param pattern ppp.
#' @param n_random Number of randomizations.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param no_change Reconstrucction will stop if energy does not decrease for this number of iterations.
#' @param annealing Probability to keep relocated point even if energy did not decrease.
#' @param comp_fast If pattern contains more points than threshold, summary functions are estimated in a computational fast way.
#' @param weights Weights used to calculate energy. The first number refers to Gest(r), the second number to pcf(r).
#' @param r_length Number of intervals from r = 0 to r = rmax the summary functions are evaluated.
#' @param return_input The original input data is returned as last list entry
#' @param simplify If n_random = 1 and return_input = FALSE only pattern will be returned.
#' @param verbose Print progress report.
#' @param plot Plot pcf function during optimization.
#'
#' @details
#' The functions randomizes the observed pattern by using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). The
#' algorithm starts with a random pattern, shifts a point to a new location and
#' keeps the change only, if the deviation between the observed and the reconstructed
#' pattern decreases. The pair correlation function and the nearest neighbour
#' distance function are used to describe the patterns.
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
#' the computational time while increasing the "bumpiness" of the summary function.
#'
#' @seealso
#' \code{\link{calculate_energy}} \cr
#' \code{\link{plot_randomized_pattern}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}} \cr
#' \code{\link{reconstruct_pattern_marks}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern_homo(species_a, n_random = 19, max_runs = 1000)
#' }
#'
#' @aliases reconstruct_pattern_homo
#' @rdname reconstruct_pattern_homo
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859-871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.
#'
#' @export
reconstruct_pattern_homo <- function(pattern,
                                     n_random = 1,
                                     e_threshold = 0.01,
                                     max_runs = 1000,
                                     no_change = Inf,
                                     annealing = 0.01,
                                     comp_fast = 1000,
                                     weights = c(0.5, 0.5),
                                     r_length = 250,
                                     return_input = TRUE,
                                     simplify = FALSE,
                                     verbose = TRUE,
                                     plot = FALSE){

  # check if n_random is >= 1
  if (n_random < 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  # check if number of points exceed comp_fast limit
  if (pattern$n > comp_fast) {

    # Print message that summary functions will be computed fast
    if (verbose) {
      message("> Using fast compuation of summary functions.")
    }

    comp_fast <- TRUE
  }

  else {
    comp_fast <- FALSE
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

  # check if weights make sense
  if (sum(weights) > 1 || sum(weights) == 0) {

    stop("The sum of 'weights' must be 0 < sum(weights) <= 1.", call. = FALSE)
  }

  # unmark pattern
  if (spatstat::is.marked(pattern)) {

    pattern <- spatstat::unmark(pattern)

    if (verbose) {
      warning("Unmarked provided input pattern. For marked pattern, see reconstruct_pattern_marks().",
              call. = FALSE)
    }
  }

  # calculate r
  r <- seq(from = 0,
           to = spatstat::rmax.rule(W = pattern$window,
                                    lambda = spatstat::intensity.ppp(pattern)),
           length.out = r_length)

  # create Poisson simulation data
  simulated <- spatstat::runifpoint(n = pattern$n,
                                    nsim = 1, drop = TRUE,
                                    win = pattern$window,
                                    warn = FALSE)

  # fast computation of summary functions
  if (comp_fast) {

    gest_observed <- spatstat::Gest(pattern, correction = "none", r = r)

    gest_simulated <- spatstat::Gest(simulated, correction = "none", r = r)

    pcf_observed <- shar::estimate_pcf_fast(pattern,
                                            correction = "none",
                                            method = "c",
                                            spar = 0.5,
                                            r = r)

    pcf_simulated <- shar::estimate_pcf_fast(simulated,
                                             correction = "none",
                                             method = "c",
                                             spar = 0.5,
                                             r = r)
  }

  # normal computation of summary functions
  else {

    gest_observed <- spatstat::Gest(X = pattern, correction = "han", r = r)

    gest_simulated <- spatstat::Gest(X = simulated, correction = "han", r = r)

    pcf_observed <- spatstat::pcf.ppp(X = pattern, correction = "best",
                                      divisor = "d",
                                      r = r)

    pcf_simulated <- spatstat::pcf.ppp(X = simulated, correction = "best",
                                       divisor = "d",
                                       r = r)
  }

  # energy before reconstruction
  energy <- (mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) * weights[[1]]) +
    (mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE) * weights[[2]])

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

    # random ids of pattern
    rp_id <- shar::rcpp_sample(x = seq_len(simulated_current$n),
                               n = max_runs, replace = TRUE)

    # create random new points
    rp_coords <- spatstat::runifpoint(n = max_runs,
                                      nsim = 1, drop = TRUE,
                                      win = simulated_current$window,
                                      warn = FALSE)

    # create random number for annealing prob
    if (annealing != 0) {

      random_annealing <- stats::runif(n = max_runs, min = 0, max = 1)
    }

    else {

      random_annealing <- rep(0, max_runs)
    }

    # pattern reconstruction algorithm (optimaztion of energy) - not longer than max_runs
    for (i in seq_len(max_runs)) {

      # data for relocation
      relocated <- simulated_current

      # get current point id
      rp_id_current <- rp_id[[i]]

      # relocate point
      relocated$x[[rp_id_current]] <- rp_coords$x[[i]]

      relocated$y[[rp_id_current]] <- rp_coords$y[[i]]

      # calculate summary functions after relocation
      if (comp_fast) {

        gest_relocated <- spatstat::Gest(relocated, correction = "none", r = r)

        pcf_relocated <- shar::estimate_pcf_fast(relocated,
                                                 correction = "none",
                                                 method = "c",
                                                 spar = 0.5,
                                                 r = r)
      }

      else {

        gest_relocated <- spatstat::Gest(X = relocated, correction = "han", r = r)

        pcf_relocated <- spatstat::pcf.ppp(X = relocated, correction = "best",
                                           divisor = "d",
                                           r = r)
      }

      # energy after relocation
      energy_relocated <- (mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm = TRUE) * weights[[1]]) +
        (mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE) * weights[[2]])

      # lower energy after relocation
      if (energy_relocated < energy_current || random_annealing[i] < annealing) {

        # keep relocated pattern
        simulated_current <- relocated

        # keep energy_relocated as energy
        energy_current <- energy_relocated

        # set counter since last change back to 0
        energy_counter <- 0

        # plot observed vs reconstructed
        if (plot) {

          # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion
          Sys.sleep(0.01)

          graphics::plot(x = pcf_observed[[1]], y = pcf_observed[[3]],
                         type = "l", col = "black",
                         xlab = "r", ylab = "g(r)")

          graphics::lines(x = pcf_relocated[[1]], y = pcf_relocated[[3]],
                          col = "red")

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
        stop_criterion_list[[current_pattern]] <- "e_threshold/no_change"

        break
      }
    }

    # close plotting device
    if (plot) {

      grDevices::dev.off()
    }

    # remove NAs if stopped due to energy
    if (stop_criterion_list[[current_pattern]] == "e_threshold/no_change") {

      energy_df <- energy_df[1:iterations, ]
    }

    # save results in lists
    energy_list[[current_pattern]] <- energy_df
    iterations_list[[current_pattern]] <- iterations
    result_list[[current_pattern]] <- simulated_current
  }

  # combine to one list
  reconstruction <- list(randomized = result_list,
                         observed = pattern,
                         method = "reconstruct_pattern_homo()",
                         energy_df = energy_list,
                         stop_criterion = stop_criterion_list,
                         iterations = iterations_list)

  # set class of result
  class(reconstruction) <- "rd_pat"

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
