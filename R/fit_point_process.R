#' fit_point_process
#'
#' @description Create random patterns by point process fitting
#'
#' @param pattern List with reconstructed patterns.
#' @param n_random Number of randomized RasterLayers.
#' @param process What point process to use. Either 'poisson' or 'cluster'
#' @param return_input The original input data is returned as last list entry
#'
#' @details
#' The functions randomizes the observed pattern by fitting a point process to the data.
#' It is possible to choose between a Poisson process or a Thomas cluster process.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pattern_fitted <- SHAR::fit_point_process(pattern_random, n_random = 9, process = "poisson")
#' }
#'
#' @aliases fit_point_process
#' @rdname fit_point_process
#'
#' @references
#' Plotkin, J. B., Potts, M. D., Leslie, N., Manokaran, N., LaFrankie, J. V., & Ashton, P. S. (2000).
#' Species-area curves, spatial aggregation, and habitat specialization in tropical forests.
#' Journal of Theoretical Biology, 207(1), 81–99.
#'
#' @export
fit_point_process <- function(pattern, n_random = 19, process = 'poisson', return_input = TRUE){

  pattern <- spatstat::unmark(pattern) # only spatial points

  # get observation window coordinates
  xrange <- pattern$window$xrange

  yrange <- pattern$window$yrange

  window <- spatstat::owin(xrange = xrange,
                           yrange = yrange)

  if(process == 'poisson'){

    result <- lapply(1:n_random, function(x) {

      spatstat::runifpoint(n = pattern$n, win = window) # simulate poisson process
    })
  }

  else if(process == 'cluster'){

    # fit cluster process
    fitted_process <- spatstat::kppm(pattern, cluster = "Thomas",
                                     statistic = "pcf",
                                     statargs = list(divisor = "d",
                                                     correction = "best"),
                                     method = "mincon",
                                     improve.type = "none")

    result <- lapply(1:n_random, function(x) {

      # create cluster community
      mobsim <- mobsim::sim_thomas_community(s_pool = 1,
                                             n_sim = pattern$n,
                                             xrange = xrange,
                                             yrange = yrange,
                                             sigma = fitted_process$modelpar[["sigma"]],
                                             cluster_points = fitted_process$modelpar[["mu"]])

      # convert to ppp
      spatstat::ppp(x = mobsim$census$x,
                    y = mobsim$census$y,
                    window = window)
      })
  }

  else{stop("Please select either 'poisson' or 'cluster'!")}

  # return input pattern and simulated patterns
  if(return_input){
    result[[n_random + 1]] <- pattern # add input raster to result list
    names(result) <-  c(rep(paste0("randomized_", 1:n_random)), "observed") # name list entries
  }

  # only return simulated patterns
  else{
    names(result) <- rep(paste0("randomized_", 1:n_random)) # name list entries
  }

  return(result)
}
