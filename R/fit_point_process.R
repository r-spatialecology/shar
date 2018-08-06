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

  if(process == 'poisson'){
    pattern_random <- pattern %>%
      spatstat::ppm() %>%
      spatstat::simulate.ppm(nsim = number_pattern, progress = F)

    while(pattern_random$n == 0) {
      pattern_random <- pattern %>%
        spatstat::ppm() %>%
        spatstat::simulate.ppm(nsim = number_pattern, progress = F)
    }
  }

  else if(process == 'cluster'){
    pattern_random <- pattern %>%
      spatstat::kppm(cluster = 'Thomas', statistic = 'pcf') %>%
      spatstat::simulate.kppm(nsim = number_pattern, verbose = FALSE)

    while(pattern_random$n == 0) {
      pattern_random <- pattern %>%
        spatstat::kppm(cluster = 'Thomas', statistic = 'pcf') %>%
        spatstat::simulate.kppm(nsim = number_pattern, verbose = FALSE)
      }
  }

  else if(process == 'softcore'){
    # pattern_random <- pattern %>%
    #   spatstat::Strauss() %>%
    #   spatstat::rStrauss(nsim = number_pattern)
    print('Not implemented yet')
  }

  else{stop('Please select either "poisson", "cluster" or "softcore" as process')}

  pattern_random[[length(pattern_random) + 1]] <- pattern
  names(pattern_random)[[length(pattern_random)]] <- "Observed"

  return(pattern_random)
}
