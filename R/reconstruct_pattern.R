#' reconstruct_pattern
#'
#' @description Pattern reconstruction
#'
#' @param pattern List with reconstructed patterns.
#' @param n_random Number of randomized RasterLayers.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param fitting It true, the pattern reconstruction starts with a fitting of a Thomas process.
#' @param comp_fast Should summary functions be estimated in an computational fast way.
#' @param verbose Print progress report.
#'
#' @details
#' The functions randomizes the observed pattern by using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). The
#' algorithm starts with a random reconstructed pattern, shifts a point to a new location and
#' keeps the change only, if the deviation between the observed and the reconstructed
#' pattern decreases. The pair correlation function and the nearest neighbour
#' distance function are used to describe the patterns. For large patterns
#' `comp_fast = TRUE` decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' @seealso
#' \code{\link{calculate_mean_energy}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pattern_recon <- SHAR::reconstruct_pattern(pattern_random, n_random = 9, max_runs = 1000)
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
reconstruct_pattern <- function(pattern, n_random = 19,
                                e_threshold = 0.01, max_runs = 10000,
                                fitting = FALSE, comp_fast = FALSE,
                                verbose = FALSE){

  pattern <- spatstat::unmark(pattern) # only spatial points

  xrange <- pattern$window$xrange
  yrange <- pattern$window$yrange

  result <- lapply(1:n_random, function(current_pattern){

    if(fitting == TRUE){ # Fit a Thomas process to the data

      fitted_process <- spatstat::kppm(pattern)

      mobsim <- mobsim::sim_thomas_community(s_pool = 1,
                                             n_sim = pattern$n,
                                             xrange = xrange,
                                             yrange = yrange,
                                             sigma = fitted_process$modelpar[["sigma"]],
                                             cluster_points = fitted_process$modelpar[["mu"]])

      simulated <- spatstat::ppp(x = mobsim$census$x,
                                 y = mobsim$census$y,
                                 window = spatstat::owin(xrange = pattern$window$xrange,
                                                         yrange = pattern$window$yrange))

    } else {simulated <- spatstat::runifpoint(n = pattern$n, win = pattern$window)} # create simulation data

    if(isTRUE(comp_fast)) {

      gest_observed <- spatstat::Gest(pattern, correction = "none")
      gest_simulated <- spatstat::Gest(simulated, correction = "none")

      pcf_observed <- SHAR::estimate_pcf_fast(pattern,
                                              correction = "none",
                                              method = "c",
                                              spar = 0.5)
      pcf_simulated <- SHAR::estimate_pcf_fast(simulated,
                                               correction = "none",
                                               method = "c",
                                               spar = 0.5)
    }

    else {

      gest_observed <- spatstat::Gest(X = pattern, correction = "han")
      gest_simulated <- spatstat::Gest(X = simulated, correction = "han")


      pcf_observed <- spatstat::pcf(X = pattern, correction = "best", divisor = "d")
      pcf_simulated <- spatstat::pcf(X = simulated, correction = "best", divisor = "d")
    }

    # energy before reconstruction
    e0 <-  mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) +
      mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE)


    for(i in 1:max_runs){ # pattern reconstruction algorithm

      relocated <- simulated # data for relocation

      rp <- sample(x = 1:relocated$n , size = 1) # random point of pattern

      relocated$x[rp] <- runif(n = 1, min = xrange[1], max = xrange[2])
      relocated$y[rp] <- runif(n = 1, min = yrange[1], max = yrange[2])

      if(isTRUE(comp_fast)) {

        gest_relocated <- spatstat::Gest(relocated, correction = "none")

        pcf_relocated <- SHAR::estimate_pcf_fast(relocated,
                                                 correction = "none",
                                                 method = "c",
                                                 spar = 0.5)
      }

      else {

        gest_relocated <- spatstat::Gest(X = relocated, correction = "han")

        pcf_relocated <- spatstat::pcf(X = relocated, correction = "best", divisor = "d")
      }

      # energy after relocation
      e_relocated <-  mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm = TRUE) +
        mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE)

      if(e_relocated < e0){ # lower energy after relocation

        simulated <- relocated # keep relocated pattern
        e0 <- e_relocated # keep e_relocated as e0
      }

      if(verbose == TRUE) {
        cat(paste0("\rProgress: n_random: ", current_pattern, "/", n_random,
                   " || max_runs: ", i, "/", max_runs,
                   " || e0 = ", round(e0, 5)))
      }

      if(e0 <= e_threshold){break} # exit loop
    }

    return(simulated)
  })

  result[[length(result) + 1]] <- pattern
  names(result) <-  c(rep(paste0("randomized_", 1:(length(result)-1))), "observed")

  return(result)
}
