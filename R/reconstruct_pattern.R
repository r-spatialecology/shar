#' reconstruct_pattern
#'
#' @description Pattern reconstruction
#'
#' @param pattern ppp.
#' @param n_random Number of randomizations.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param no_change Reconstrucction will stop if energy does not decrease for this number of iterations.
#' @param fitting It true, the pattern reconstruction starts with a fitting of a Thomas process.
#' @param comp_fast If pattern contains more points than threshold, summary functions are estimated in a computational fast way.
#' @param return_input The original input data is returned as last list entry
#' @param simplify If n_random = 1 and return_input = FALSE only pattern will be returned.
#' @param verbose Print progress report.
#' @param plot Plot pcf function during optimization.
#'
#' @details
#' The functions randomizes the observed pattern by using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). The
#' algorithm starts with a random reconstructed pattern, shifts a point to a new location and
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
#' @seealso
#' \code{\link{calculate_energy}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @return list
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
#' patterns. Computational Statistics and Data Analysis, 51(2), 859–871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.
#'
#' @export
reconstruct_pattern <- function(pattern,
                                n_random = 1,
                                e_threshold = 0.01,
                                max_runs = 1000,
                                no_change = Inf,
                                fitting = FALSE,
                                comp_fast = 1000,
                                return_input = TRUE,
                                simplify = FALSE,
                                verbose = TRUE,
                                plot = FALSE){

  # check if n_random is >= 1
  if(!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  # check if number of points exceed comp_fast limit
  if(pattern$n > comp_fast) {

    # Print message that summary functions will be computed fast
    if(verbose) {
      message("> Using fast compuation of summary functions.")
    }

    comp_fast <- TRUE
  }

  else {
    comp_fast <- FALSE
  }

  # counter if energy changed
  energy_counter <- 0

  # unmark pattern
  if(spatstat::is.marked(pattern)) {

    pattern <- spatstat::unmark(pattern)

    warning("Unmarked provided input pattern. For marked pattern, see reconstruct_marks().",
            call. = FALSE)
  }

  # calculate r
  r <- seq(from = 0,
           to = spatstat::rmax.rule(W = pattern$window,
                                    lambda = spatstat::intensity.ppp(pattern)),
           length.out = 250)

  # start with fitted pattern
  if(fitting) {

    # fit Thomas process
    fitted_process <- spatstat::kppm.ppp(pattern, cluster = "Thomas",
                                         statistic = "pcf",
                                         statargs = list(divisor = "d",
                                                         correction = "best"),
                                         method = "mincon",
                                         improve.type = "none")

    # simulte clustered pattern
    simulated <- spatstat::simulate.kppm(fitted_process,
                                         nsim = 1, drop = TRUE,
                                         window = pattern$window,
                                         verbose = FALSE)

    # remove points because more points in simulated
    if(pattern$n < simulated$n) {

      # difference between patterns
      difference <- simulated$n - pattern$n

      # id of points to remove
      remove_points <- rcpp_sample(x = seq_len(simulated$n), n = difference)

      # remove points
      simulated <- simulated[-remove_points]
    }

    # add points because less points in simulated
    if(pattern$n > simulated$n) {

      # difference between patterns
      difference <- pattern$n - simulated$n

      # create missing points
      missing_points <- spatstat::runifpoint(n = difference,
                                             nsim = 1, drop = TRUE,
                                             win = pattern$window,
                                             warn = FALSE)

      # add missing points to simulated
      simulated <- spatstat::superimpose.ppp(simulated, missing_points,
                                             W = pattern$window, check = FALSE)
    }
  }

  # create Poisson simulation data
  else {
    simulated <- spatstat::runifpoint(n = pattern$n,
                                      nsim = 1, drop = TRUE,
                                      win = pattern$window,
                                      warn = FALSE)
  }

  # fast computation of summary functions
  if(comp_fast) {

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
  energy <- mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) +
    mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE)

  # create n_random recondstructed patterns
  result <- lapply(seq_len(n_random), function(current_pattern) {

    # random ids of pattern
    rp_id <- rcpp_sample(x = seq_len(pattern$n),
                         n = max_runs, replace = TRUE)

    # create random new points
    rp_coords <- spatstat::runifpoint(n = max_runs,
                                      nsim = 1, drop = TRUE,
                                      win = pattern$window,
                                      warn = FALSE)

    # pattern reconstruction algorithm (optimaztion of energy) - not longer than max_runs
    for(i in seq_len(max_runs)) {

      # data for relocation
      relocated <- simulated

      # get current point id
      rp_id_current <- rp_id[[i]]

      # relocate point
      relocated$x[[rp_id_current]] <- rp_coords$x[[i]]

      relocated$y[[rp_id_current]] <- rp_coords$y[[i]]

      # calculate summary functions after relocation
      if(comp_fast) {

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
      e_relocated <- mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm = TRUE) +
        mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE)

      # lower energy after relocation
      if(e_relocated < energy) {

        # keep relocated pattern
        simulated <- relocated

        # keep e_relocated as energy
        energy <- e_relocated

        # set counter since last change back to 0
        energy_counter <- 0

        # plot observed vs reconstructed
        if(plot) {

          # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion
          Sys.sleep(0.1)

          graphics::plot(x = pcf_observed[[1]], y = pcf_observed[[3]],
                         type = "l", col = "black",
                         xlab = "r", ylab = "g(r)")

          graphics::lines(x = pcf_relocated[[1]], y = pcf_relocated[[3]], col = "red")

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

      # print progress
      if(verbose) {
        message("\r> Progress: n_random: ", current_pattern, "/", n_random,
                " || max_runs: ", i, "/", max_runs,
                " || energy = ", round(energy, 5),
                appendLF = FALSE)
      }

      # exit loop if e threshold or no_change counter max is reached
      if(energy <= e_threshold || energy_counter > no_change) {
        break
      }
    }

    return(simulated)
  })

  # add input pattern to randomizations
  if(return_input){

    # simplify not possible if input pattern should be returned
    if(simplify){
      message("\n")
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)
    }

    # add input pattern as last list entry
    result[[n_random + 1]] <- pattern

    # set names
    names(result) <-  c(paste0("randomized_", seq_len(n_random)), "observed")
  }

  # don't add input pattern
  else{

    # don't return list
    if(simplify) {

      # simplify not possible if more than one random pattern is present
      if(n_random > 1) {
        message("\n")
        warning("'simplify = TRUE' not possible for 'n_random > 1'.", call. = FALSE)
      }

      # return only pattern not as list
      else {
        result <- result[[1]]
      }
    }

    # set names of list to return
    else{
      names(result) <- paste0("randomized_", seq_len(n_random))
    }
  }

  # write result in new line if progress was printed
  if(verbose) {
    message("\r")
  }

  return(result)
}
