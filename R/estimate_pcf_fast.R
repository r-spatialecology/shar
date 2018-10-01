#' estimate_pcf_fast
#'
#' @description Fast estimation of the pair correlation function
#'
#' @param pattern Point pattern
#' @param ... Arguments passed to \code{\link{Kest}} and \code{\link{pcf.fv}}
#'
#' @details
#' Classifies a RasterLayer with continious values into n discrete classes
#'
#' @seealso
#' \code{\link{Kest}} \cr
#' \code{\link{pcf.fv}}
#'
#' @return fv.object
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pcf_pattern_random <- estimate_pcf_fast(pattern_random)
#' }
#'
#' @aliases estimate_pcf_fast
#' @rdname estimate_pcf_fast

#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- spatstat::Kest(X = pattern, ...)

  result <- spatstat::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(result)
}
