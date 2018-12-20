#' fit_point_process
#'
#' @description Create random patterns by point process fitting
#'
#' @param pattern List with reconstructed patterns.
#' @param n_random Number of randomized RasterLayers.
#' @param process What point process to use. Either 'poisson' or 'cluster'.
#' @param return_input The original input data is returned as last list entry.
#' @param simplify If n_random = 1 and return_input = FALSE only pattern will be returned.
#' @param verbose Print progress report.
#'
#' @details
#' The functions randomizes the observed pattern by fitting a point process to the data.
#' It is possible to choose between a Poisson process or a Thomas cluster process.
#'
#' @return list
#'
#' @examples
#' pattern_fitted <- fit_point_process(species_a, n_random = 39)
#'
#' @aliases fit_point_process
#' @rdname fit_point_process
#'
#' @references
#' Plotkin, J. B., Potts, M. D., Leslie, N., Manokaran, N., LaFrankie, J. V., & Ashton, P. S. (2000).
#' Species-area curves, spatial aggregation, and habitat specialization in tropical forests.
#' Journal of Theoretical Biology, 207(1), 81–99.

#' @export
fit_point_process <- function(pattern,
                              n_random = 19, process = 'poisson',
                              return_input = TRUE, simplify = FALSE, verbose = FALSE){

  # check if n_random is >= 1
  if(!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

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

  else{

    stop("Please select either 'poisson' or 'cluster'.", call. = FALSE)
  }

  # add input pattern to randomizations
  if(return_input){

    if(verbose & simplify){
      cat("\n")
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'", call. = FALSE)
    }

    result[[n_random + 1]] <- pattern # add input pattern as last list entry

    names(result) <-  c(paste0("randomized_", 1:n_random), "observed") # set names
  }

  else{

    if(simplify) {

      if(verbose & n_random > 1) {
        cat("\n")
        warning("'simplify = TRUE' not possible for 'n_random > 1'", call. = FALSE)
      }

      else {
        result <- result[[1]]
      }
    }

    else{
      names(result) <- paste0("randomized_", 1:n_random) # set names
    }
  }

  return(result)
}
