#' Help function to calculate enegry
#'
#' @param pattern [\code{list(1)}] List with reconstructed point patterns created with SHAR::Pattern.Reconstruction()
#' @param gest_observed [\code{fv(1)}] Neareast neighbour distribution functions o observed pattern
#' @param pcf_observed [\code{fv(1)}] Pair correlation function of observed pattern
#' @param only_spatial [\code{logical(1)}] TRUE for univariate point pattern

#' @export
Calculate.Energy <- function(pattern, gest_observed, pcf_observed, only_spatial){
  if(only_spatial==T){
    gest_i <- spatstat::Gest(X=pattern, correction="best")

    if(pattern$n>=500){pcf_i <- SHAR::Pcf.Fast(pattern=pattern)}
    else{pcf_i <- spatstat::pcf.ppp(X=pattern, divisor='d', correction="Ripley")}

    e_gest <- c(mean(abs(gest_observed[[3]] - gest_i[[3]]), na.rm=T))
    e_pcf <- c(mean(abs(pcf_observed[[3]] - pcf_i[[3]]), na.rm=T))
  }
  else{
    gest_i <- SHAR::Gest.Multi(pattern=pattern)
    pcf_i <- SHAR::Pcf.Multi(pattern=pattern)

    e_gest <- c(mean(abs(gest_observed$Mean - gest_i$Mean), na.rm=T))
    e_pcf<- c(mean(abs(pcf_observed$Mean - pcf_i$Mean), na.rm=T))
  }
  dplyr::bind_cols(e_gest=e_gest, e_pcf=e_pcf)
}

