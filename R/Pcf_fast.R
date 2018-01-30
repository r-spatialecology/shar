#' pcf-function
#'
#' Computational fast method to calculate the pair correlation function using Ripleys K-Function
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat packages
#' @param correction [\code{string(1)}]\cr Edge correction (see ?spatstat::Kest() for more details)
#' @param method [\code{character(1)}]\cr See spatstat::pcf.fv() for mor details
#' @param ... [\code{various(1)}]\cr Parameters given to spatstat::Kest() or  spatstat::pcf.fv()
#' @return fv object of the spatstat package

#' @export
Pcf.Fast <- function(pattern, correction="good", method="d", ...){
  kest_pattern <- spatstat::Kest(pattern, correction=correction, ...)
  pcf_pattern <- spatstat::pcf.fv(kest_pattern, method=method, spar=0.5, ...)
  return(pcf_pattern)
}
