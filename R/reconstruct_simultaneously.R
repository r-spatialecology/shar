#' #' Pattern reconstruction
#' #'
#' #' Multivariate pattern reconstruction using a simultaneously approach
#' #' @param pattern [\code{ppp(1)}] Multivariate ppp object of the spatstat package
#' #' @param max_runs [\code{numeric(1)}] number of maximum iterations
#' #' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' #' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#' #'
#' #' @return ppp object of the spatstat package with reconstructed pattern
#'
#' #' @export
#' reconstruct_simultaneously <- function(pattern, max_runs = 10000, e_threshold = 0.01, fitting = FALSE){
#'
#'   pattern <- SHAR::select_species(pattern) # data to reconstruct
#'
#'   if(fitting == TRUE){ # Fit Thomas process to data
#'     simulated <- pattern %>%
#'       spatstat::unmark() %>%
#'       spatstat::kppm(cluster = 'Thomas', statistic = 'pcf') %>%
#'       spatstat::simulate.kppm(window = pattern$window, drop = TRUE, verbose = FALSE)
#'
#'     if(simulated$n != pattern$n){ # Not the same number of points fitted and observed data
#'       dif <- abs(pattern$n - simulated$n)
#'       if(simulated$n < pattern$n){ # Add random points
#'         p <- spatstat::runifpoint(n = dif, win = pattern$window)
#'         simulated <- spatstat::superimpose(simulated, p)
#'       }
#'       else{ # remove random points
#'         r <- sample(1:max(simulated$n), size = dif, replace = FALSE)
#'         simulated <- simulated[-r]
#'       }
#'     }
#'   }
#'   else{simulated <- spatstat::runifpoint(n = spatstat::unmark(pattern)$n,
#'                                          win = spatstat::unmark(pattern)$window)} # create simulation data
#'
#'   species <- rep(levels(pattern$marks), spatstat::summary.ppp(pattern)$marks[[1]]) # get marks
#'   spatstat::marks(simulated) <- factor(species) # assign marks to simulated pattern
#'
#'   if(pattern$n >= 1000){ # indirect computation because spatstat::pcf() is too slow
#'     pcf_observed <- SHAR::estimate_pcf_fast(pattern)
#'     pcf_simulated <- SHAR::estimate_pcf_fast(simulated)
#'   }
#'
#'   else{ # direct computation
#'     pcf_observed <- spatstat::pcf(pattern, correction = 'best', divisor = 'd') # g(r) observed data
#'     pcf_simulated <- spatstat::pcf(simulated, correction = 'best', divisor = 'd') # g(r) simulated data
#'   }
#'
#'   gest_observed <- spatstat::Gest(pattern, correction = 'best') # G(r) observed data
#'   gest_simulated <- spatstat::Gest(simulated, correction = 'best') # g(r) simulated pattern
#'
#'   pcfmulti_observed <- SHAR::estimate_pcf_multi(pattern) # pcfmulti observed data
#'   pcfmulti_simulated <- SHAR::estimate_pcf_multi(simulated) # pcfmulti simulated data
#'
#'   gmulti_observed <- SHAR::estimate_gest_multi(pattern) # Gmulti(r) observed data
#'   gmulti_simulated <- SHAR::estimate_gest_multi(simulated) # Gmulti(r) simulated data
#'
#'   e0_pcf <- mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE) # energy g(r)
#'   e0_gest <- mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) # energy G(r)
#'   e0_pcfmulti <- mean(abs(pcfmulti_observed$Mean - pcfmulti_simulated$Mean), na.rm = TRUE) # energy isar
#'   e0_gmulti <- mean(abs(gmulti_observed$Mean - gmulti_simulated$Mean), na.rm = TRUE) # energy Gmulti
#'
#'   # Add weights
#'   e0 <- e0_pcf + e0_gest + e0_pcfmulti + e0_gmulti # overall energy
#'
#'   for(i in 1:max_runs){ # pattern reconstruction
#'     relocated <- simulated # create relocation data
#'
#'     rp <- sample(size = 1, 1:relocated$n) # random point of pattern
#'     relocated_temp <- relocated[-rp] # remove point from pattern
#'     point <- spatstat::runifpoint(n = 1, win = relocated$window) # create random coordinates
#'     spatstat::marks(point) <- spatstat::marks(relocated[rp]) # assign marks
#'     relocated <- spatstat::superimpose(relocated_temp, point) # add point to pattern
#'
#'     if(relocated$n >= 1000){ # indirect computation
#'       pcf_relocated <- SHAR::estimate_pcf_fast(relocated)
#'       # k_relocated <- spatstat::Kest(relocated, correction = 'good') # K(r) after relocation
#'       # pcf_relocated <- spatstat::pcf.fv(k_relocated, spar = 0.5, method = 'd') # g(r) after relocation
#'     }
#'     else{pcf_relocated <- spatstat::pcf(relocated, correction = 'best', divisor = 'd')} # g(r) after relocation
#'
#'     gest_relocated <- spatstat::Gest(relocated, correction = 'best') # G(r) after relocation
#'     pcfmulti_relocated <- SHAR::estimate_pcf_multi(relocated) # pcf after relocation
#'     gmulti_relocated <- SHAR::estimate_gest_multi(relocated) # Gmulti(r) simulated data
#'
#'     e_relocated_pcf <- mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE) # energy g(r) after relocation
#'     e_relocated_gest <- mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm = TRUE) # energy G(r) after relocation
#'     e_relocated_pcfmulti <- mean(abs(pcfmulti_observed$Mean - pcfmulti_relocated$Mean), na.rm = TRUE) # energy iSAR after relocation
#'     e_relocated_gmulti <- mean(abs(gmulti_observed$Mean - gmulti_relocated$Mean), na.rm = TRUE) # energy Gmulti
#'
#'     # Add weights
#'     e_relocated <- e_relocated_pcf + e_relocated_gest + e_relocated_pcfmulti + e_relocated_gmulti # total energy after relocation
#'
#'     if(e_relocated < e0){ # lower energy after relocation
#'       simulated <- relocated # keep relocated pattern
#'       e0 <- e_relocated # keep e_relocated
#'       pcf_simulated <- pcf_relocated # keep pcf_relocated
#'       gest_simulated <- gest_relocated # keep Gest_relocated
#'       pcfmulti_simulated <- pcfmulti_relocated # keep iSAR
#'       gmulti_simulated <- gmulti_relocated # keep Gmulti(r)
#'     }
#'
#'     if(e0 <= e_threshold){break}
#'   }
#'
#'   return(simulated) # return results
#' }
