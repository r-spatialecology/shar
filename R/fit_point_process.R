#' Gamma test
#'
#' @param input [\code{ppp(1)}] ppp object of the spatstat package
#' @param process [\code{string(1)}] Method used for the reconstruction. Either "Poisson", "Cluster" or "Softcore"
#' @param number_pattern [\code{numeric(1)}] Number of reconstructed patterns

#' @return List containing reconstructed patterns and observed pattern

#' @export
fit_point_process <- function(input, process = 'poisson', number_pattern = 199){

  pattern <- input %>%
    spatstat::unmark()

  pattern_random <- vector("list", length = number_pattern + 1)

  names(pattern_random) <- c(paste0("Simulation_", 1:number_pattern), "Observed")

  if(process == 'poisson'){

    point_process <- spatstat::ppm(pattern)

    for(i in 1:number_pattern) {

      sim_pattern <- spatstat::simulate.ppm(object = point_process,
                                            nsim = 1,
                                            drop = TRUE, progress = FALSE)

      while(sim_pattern$n == 0) {

        sim_pattern <- spatstat::simulate.ppm(object = point_process,
                                              nsim = 1,
                                              drop = TRUE, progress = FALSE)
      }

      pattern_random[[i]] <- sim_pattern
    }
  }

  else if(process == 'cluster'){

    point_process <-spatstat::kppm(X = pattern,
                                   cluster = 'Thomas', statistic = 'pcf')

    for(i in 1:number_pattern) {

      sim_pattern <- spatstat::simulate.kppm(object = point_process,
                                             nsim = 1,
                                             drop = TRUE, verbose = FALSE)
      while(sim_pattern$n == 0) {
        sim_pattern <- spatstat::simulate.kppm(object = point_process,
                                               nsim = 1,
                                               drop = TRUE, verbose = FALSE)
      }

      pattern_random[[i]] <- sim_pattern
    }
  }

  else if(process == 'softcore'){
    # pattern_random <- pattern %>%
    #   spatstat::Strauss() %>%
    #   spatstat::rStrauss(nsim = number_pattern)
    print('Not implemented yet')
  }

  else{stop('Please select either "poisson", "cluster" or "softcore" as process')}

  pattern_random[[number_pattern + 1]] <- pattern

  return(pattern_random)
}
