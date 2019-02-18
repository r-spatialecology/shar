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
                              n_random = 1, process = 'poisson',
                              return_input = TRUE,
                              simplify = FALSE,
                              verbose = TRUE){

  # check if n_random is >= 1
  if(!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  pattern <- spatstat::unmark(pattern) # only spatial points

  if(process == "poisson"){

    result <- lapply(seq_len(n_random), function(x) {

      simulated <- spatstat::runifpoint(n = pattern$n,
                                        win = pattern$window) # simulate poisson process


      if(verbose) {
        message("\r> Progress: n_random: ", x, "/", n_random, appendLF = FALSE)
      }

      return(simulated)
    })
  }

  else if(process == "cluster"){

    # fit cluster process
    fitted_process <- spatstat::kppm(pattern, cluster = "Thomas",
                                     statistic = "pcf",
                                     statargs = list(divisor = "d",
                                                     correction = "best"),
                                     method = "mincon",
                                     improve.type = "none")

    result <- lapply(seq_len(n_random), function(x) {

      # simulte clustered pattern
      simulated <- spatstat::simulate.kppm(fitted_process,
                                           window = pattern$window,
                                           nsim = 1, drop = TRUE)

      # remove points because more points in simulated
      if(pattern$n < simulated$n) {

        # difference between patterns
        difference <- simulated$n - pattern$n

        # id of points to remove
        remove_points <- shar::rcpp_sample(x = seq_len(simulated$n), n = difference)

        # remove points
        simulated <- simulated[-remove_points]
      }

      # add points because less points in simulated
      else if(pattern$n > simulated$n) {

        # difference between patterns
        difference <- pattern$n - simulated$n

        # create missing points
        missing_points <- spatstat::runifpoint(n = difference,
                                               win = pattern$window,
                                               nsim = 1, drop = TRUE)

        # add missing points to simulated
        simulated <- spatstat::superimpose(simulated, missing_points,
                                           W = pattern$window)
      }

      if(verbose) {
        message("\r> Progress: n_random: ", x, "/", n_random, appendLF = FALSE)
      }

      return(simulated)
    })
  }

  else{
    stop("Please select either 'poisson' or 'cluster'.", call. = FALSE)
  }

  # add input pattern to randomizations
  if(return_input){

    if(simplify && verbose){
      message("\n")
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)
    }

    result[[n_random + 1]] <- pattern # add input pattern as last list entry

    names(result) <-  c(paste0("randomized_", seq_len(n_random)), "observed") # set names
  }

  else{

    if(simplify) {

      if(n_random > 1 && verbose) {
        message("\n")
        warning("'simplify = TRUE' not possible for 'n_random > 1'.", call. = FALSE)
      }

      else {
        result <- result[[1]]
      }
    }

    else{
      names(result) <- paste0("randomized_", seq_len(n_random)) # set names
    }
  }

  # write result in new line if progress was printed
  if(verbose) {
    message("\r")
  }

  return(result)
}
