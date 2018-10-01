#' reconstruct_pattern
#'
#' @description Pattern reconstruction
#'
#' @param pattern List with reconstructed patterns
#' @param comp_fast Logical if summary functions should be estimated in an computational
#' fast way (but without edge correction)
#' @param n_random Number of randomized RasterLayers
#' @param verbose Print progress report
#' @param max_runs Maximum number of iterations of e_threshold is not reached
#' @param e_threshold Minimum energy to stop reconstruction
#' @param fitting It true, the pattern reconstruction starts with a fitting of a Thomas process
#'
#' @details
#' Pattern reconstruction
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
                                max_runs = 10000, e_threshold = 0.01,
                                fitting = FALSE, verbose = FALSE, comp_fast = FALSE){

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

    # energy before reconstruction
    e0 <-  mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) +
      mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE)


    for(i in 1:max_runs){ # pattern reconstruction algorithm

      relocated <- simulated # data for relocation

      rp <- sample(x = 1:relocated$n , size = 1) # random point of pattern

      relocated$x[rp] <- runif(n = 1, min = xrange[1], max = xrange[2])
      relocated$y[rp] <- runif(n = 1, min = yrange[1], max = yrange[2])


      gest_relocated <- spatstat::Gest(relocated, correction = "none")

      pcf_relocated <- SHAR::estimate_pcf_fast(relocated,
                                               correction = "none",
                                               method = "c",
                                               spar = 0.5)

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
