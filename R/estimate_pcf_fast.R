#' pcf-function
#'
#' Computational fast method to calculate the pair correlation function using Ripleys K-Function
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat packages
#' @param correction [\code{string(1)}]\cr Edge correction (see ?spatstat::Kest() for more details)
#' @param method [\code{character(1)}]\cr See spatstat::pcf.fv() for mor details
#' @param spar [\code{numeric(1)}]\cr Smooting of spline function (see ?stats::smooth.spline() for more details)
#'
#' @return fv object of the spatstat package

#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- spatstat::Kest(X = pattern, ...)

  pcf_fun <- spatstat::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(pcf_fun)
}

# correction = 'good', method = 'c', spar = 0.35
