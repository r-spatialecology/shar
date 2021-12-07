#' estimate_pcf_fast
#'
#' @description Fast estimation of the pair correlation function
#'
#' @param pattern ppp object with point pattern.
#' @param ... Arguments passed down to \code{link{Kest}} or \code{\link{pcf.fv}}.
#'
#' @details
#' The functions estimates the pair correlation functions based on an estimation
#' of Ripley's K-function. This makes it computationally faster than estimating the
#' pair correlation function directly. It is a wrapper around \code{\link{Kest}} and
#' \code{\link{pcf.fv}}.
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
#' Chiu, S.N., Stoyan, D., Kendall, W.S., Mecke, J., 2013. Stochastic geometry and
#' its applications, 3rd ed, Wiley Series in Probability and Statistics.
#' John Wiley & Sons Inc, Chichester, UK. <isbn:978-0-470-66481-0>
#'
#' Ripley, B.D., 1977. Modelling spatial patterns. Journal of the Royal Statistical
#' Society. Series B (Methodological) 39, 172–192.
#' <https://doi.org/10.1111/j.2517-6161.1977.tb01615.x>
#'
#' Stoyan, D., Stoyan, H., 1994. Fractals, random shapes and point fields.
#' John Wiley & Sons, Chichester. <isbn:978-0-471-93757-9>
#'
#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- suppressMessages(spatstat.core::Kest(X = pattern, ...)) # estimate K-fct

  result <- spatstat.core::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(result)
}
