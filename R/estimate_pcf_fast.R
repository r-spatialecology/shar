#' estimate_pcf_fast
#'
#' @description Fast estimation of the pair correlation function
#'
#' @param pattern Point pattern.
#' @param ... Arguments passed down to `Kest` or `pcf.fv`.
#'
#' @details
#' The functions estimates the pair correlation functions based on an estimation
#' of Ripley's K-function. This makes it computationally faster than estimating the
#' pair correlation function directly. It is a wrapper around `Kest` and `pcf.fv`.
#'
#' @seealso
#' \code{\link{Kest}} \cr
#' \code{\link{pcf.fv}}
#'
#' @return fv.object
#'
#' @examples
#' pcf_species_b <- estimate_pcf_fast(species_a)
#'
#' @aliases estimate_pcf_fast
#' @rdname estimate_pcf_fast
#'
#' @references
#' Ripley, B.D. (1977) Modelling spatial patterns (with discussion). Journal of
#' the Royal Statistical Society, Series B, 39, 172-212.
#'
#' Stoyan, D, Kendall, W.S. and Mecke, J. (1995) Stochastic geometry and its
#' applications. 2nd edition. Springer Verlag.
#'
#' Stoyan, D. and Stoyan, H. (1994) Fractals, random shapes and point fields:
#' methods of geometrical statistics. John Wiley and Sons.

#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- suppressMessages(spatstat::Kest(X = pattern, ...)) # estimate K-fct

  result <- spatstat::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(result)
}
