#' Help function to calculate enegry
#'
#' @param pattern [\code{list(1)}] List with reconstructed point patterns created with SHAR::Pattern.Reconstruction()
#' @param gest_observed [\code{fv(1)}] Neareast neighbour distribution functions o observed pattern
#' @param pcf_observed [\code{fv(1)}] Pair correlation function of observed pattern
#' @param only_spatial [\code{logical(1)}] TRUE for univariate point pattern

#' @export
calculate_energy <- function(pattern, gest_observed, pcf_observed, only_spatial){
  if(only_spatial == TRUE){
    gest_i <- spatstat::Gest(X = pattern, correction = 'best')

    if(pattern$n >= 1000){pcf_i <- SHAR::estimate_pcf_fast(pattern = pattern)}
    else{pcf_i <- spatstat::pcf.ppp(X = pattern, divisor = 'd', correction = 'Ripley')}

    e_gest <- c(mean(abs(gest_observed[[3]] - gest_i[[3]]), na.rm = TRUE))
    e_pcf <- c(mean(abs(pcf_observed[[3]] - pcf_i[[3]]), na.rm = TRUE))
  }
  else{
    gest_i <- SHAR::estimate_gest_multi(pattern = pattern)
    pcf_i <- SHAR::estimate_pcf_multi(pattern = pattern)

    e_gest <- c(mean(abs(gest_observed$Mean - gest_i$Mean), na.rm = TRUE))
    e_pcf<- c(mean(abs(pcf_observed$Mean - pcf_i$Mean), na.rm = TRUE))
  }
  dplyr::bind_cols(e_gest = e_gest, e_pcf = e_pcf)
}

