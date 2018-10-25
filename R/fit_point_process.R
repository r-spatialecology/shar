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

  if(process == 'poisson'){

    fitted_process <- spatstat::ppm(pattern)

    result <- lapply(1:n_random, function(x) {

      mobsim <- mobsim::sim_poisson_community(s_pool = 1,
                                              n_sim = pattern$n,
                                              xrange = xrange,
                                              yrange = yrange)

      spatstat::ppp(x = mobsim$census$x,
                    y = mobsim$census$y,
                    window = spatstat::owin(xrange = pattern$window$xrange,
                                            yrange = pattern$window$yrange))
    })
  }

  else if(process == 'cluster'){

    fitted_process <- spatstat::kppm(pattern)

    result <- lapply(1:n_random, function(x) {

      mobsim <- mobsim::sim_thomas_community(s_pool = 1,
                                             n_sim = pattern$n,
                                             xrange = xrange,
                                             yrange = yrange,
                                             sigma = fitted_process$modelpar[["sigma"]],
                                             cluster_points = fitted_process$modelpar[["mu"]])

      spatstat::ppp(x = mobsim$census$x,
                    y = mobsim$census$y,
                    window = spatstat::owin(xrange = pattern$window$xrange,
                                            yrange = pattern$window$yrange))
      })
  }

  else{stop("Please select either 'poisson' or 'cluster'!")}

  if(isTRUE(return_input)){
    result[[n_random + 1]] <- pattern
    names(result) <-  c(rep(paste0("randomized_", 1:n_random)), "observed")
  }

  else{
    names(result) <- rep(paste0("randomized_", 1:n_random))
  }

  return(result)
}
