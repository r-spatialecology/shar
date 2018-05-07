#' Mean energy of the reconstruction
#'
#' Function to plot all reconstructed patterns created of the pattern list with reconstructed patterns
#' @param pattern [\code{list(1)}] List with reconstructed and observed point pattern created with SHAR::Pattern.Reconstruction()
#' @param only_spatial [\code{logical(1)}] TRUE for univariate point pattern
#'
#' @return Numeric with mean energy of the reconstruction

#' @export
calculate_mean_energy <- function(pattern, only_spatial = TRUE){

  pattern_observed <- pattern[names(pattern) == 'Observed']
  pattern_reconstructed <- pattern[names(pattern) != 'Observed']

  if(only_spatial == T){
    gest_observed <- spatstat::Gest(X = pattern_observed[[1]], correction = 'best')

    if(pattern_observed[[1]]$n >= 1000){pcf_observed <- SHAR::estimate_pcf_fast(pattern = pattern_observed[[1]])}
    else{pcf_observed <- spatstat::pcf.ppp(X = pattern_observed[[1]],
                                           divisor = 'd', correction = 'Ripley')}

    result <- purrr::map_dfr(pattern_reconstructed, function(x) SHAR::calculate_energy(pattern = x,
                                                                                       only_spatial = only_spatial,
                                                                                       gest_observed = gest_observed,
                                                                                       pcf_observed = pcf_observed)) %>%
      colMeans(na.rm = TRUE)
  }

  else{
    gest_observed <- SHAR::estimate_gest_multi(pattern = pattern_observed[[1]])
    pcf_observed <- SHAR::estimate_pcf_multi(pattern = pattern_observed[[1]])

    result <- purrr::map_dfr(pattern_reconstructed, function(x) SHAR::calculate_energy(pattern = x,
                                                                                       only_spatial = only_spatial,
                                                                                       gest_observed = gest_observed,
                                                                                       pcf_observed = pcf_observed)) %>%
      colMeans(na.rm = TRUE)
  }
  return(result)
}

