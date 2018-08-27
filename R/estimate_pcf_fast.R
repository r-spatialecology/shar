#' pcf-function
#'
#' Computational fast method to calculate the pair correlation function using Ripleys K-Function
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat packages
#' @param ... options passed to for Kest() and pcf.fv()
#'
#' @return fv object of the spatstat package

#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- spatstat::Kest(X = pattern, ...)

  pcf_fun <- spatstat::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(pcf_fun)
}
