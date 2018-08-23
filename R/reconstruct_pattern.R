#' Spatial reconstruction
#'
#' Univariate pattern reconstruction of only the spatial structure
#' @param pattern [\code{ppp(1)}]\cr Multivariate ppp object of the spatstat package
#' @param max_runs [\code{numeric(1)}]\cr number of maximum iterations
#' @param e_threshold [\code{numeric(1)}]\cr Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}]\cr If TRUE, a clustered pattern is fitted to the original pattern as starting point
#' @param number_reconstructions [\code{numeric(1)}] Number of reconstructed patterns
#'
#' @return ppp object of the spatstat package with reconstructed pattern

#' @export
reconstruct_pattern <- function(pattern, number_reconstructions = 1,
                                max_runs = 10000, e_threshold = 0.01,
                                fitting = FALSE){

  pattern <- spatstat::unmark(pattern) # only spatial points

  x_range <- pattern$window$xrange
  y_range <- pattern$window$yrange

  result <- purrr::map(1:number_reconstructions, function(current_pattern){

    if(fitting == T){ # Fit a Thomas process to the data

      fitted_process <- kppm(pattern)

      window_pattern <- pattern$window$xrange

      mobsim <- mobsim::sim_thomas_community(s_pool = 1,
                                             n_sim = pattern$n,
                                             xrange = c(0, 1),
                                             yrange = c(0, 1),
                                             sigma = fitted_process$modelpar[["sigma"]],
                                             cluster_points = fitted_process$modelpar[["mu"]])

      simulated <- ppp(x = mobsim$census$x,
                       y = mobsim$census$y,
                       window = owin(xrange = pattern$window$xrange,
                                     yrange = pattern$window$yrange))
    }

    else{simulated <- spatstat::runifpoint(n = pattern$n, win = pattern$window)} # create simulation data

    pcf_observed <- SHAR::estimate_pcf_fast(pattern, correction = "none")
    pcf_simulated <- SHAR::estimate_pcf_fast(simulated, correction = "none")

    e0_pcf <- mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE) # energy g(r)

    for(i in 1:max_runs){ # pattern reconstruction algorithm

      relocated <- simulated # data for relocation

      rp <- sample(x = 1:relocated$n , size = 1) # random point of pattern

      relocated$x[rp] <- runif(n = 1, min = x_range[1], max = x_range[2])
      relocated$y[rp] <- runif(n = 1, min = y_range[1], max = y_range[2])

      pcf_relocated <- SHAR::estimate_pcf_fast(relocated, correction = "none")

      e_relocated_pcf <- mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE) # energy after relocation

      if(e_relocated_pcf < e0_pcf){ # lower energy after relocation

        simulated <- relocated # keep relocated pattern
        e0_pcf <- e_relocated_pcf # keep e_relocated as e0
      }

      if(e0_pcf <= e_threshold){break} # exit loop
    }

    return(simulated)
  })

  result[[length(result) + 1]] <- pattern
  names(result) <-  c(rep(paste0('Randomized_', 1:(length(result)-1))),
                      'Observed')

  return(result)
}
