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
estimate_pcf_fast <- function(pattern, correction = 'good', method = 'c', spar = 0.35){
  result <- pattern %>%
    spatstat::Kest(correction = correction) %>% # estimate K-fct
    spatstat::pcf.fv(method = method, spar = spar) # estimate pcf from K-fct
  return(result)
}
