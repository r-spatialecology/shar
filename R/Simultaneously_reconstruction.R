#' Pattern reconstruction
#'
#' Multivariate pattern reconstruction using a simultaneously approach
#' @param pattern [\code{ppp(1)}] Multivariate ppp object of the spatstat package
#' @param max_runs [\code{numeric(1)}] number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#'
#' @return ppp object of the spatstat package with reconstructed pattern

#' @export
Simultaneously.Reconstruction <- function(pattern, max_runs, e_threshold, fitting){

  pattern <- spatstat::subset.ppp(pattern, select=Species) # data to reconstruct

  if(fitting==T){
    fit_mat <- spatstat::kppm(spatstat::unmark(pattern), clusters="MatClust", statistic="pcf")
    simulated <- spatstat::rMatClust(kappa=fit_mat$par[[1]],
                                     scale=fit_mat$par[[2]],
                                     mu=fit_mat$mu,
                                     win=pattern$window) # create simulation data

    if(simulated$n != pattern$n){
      dif <- abs(pattern$n -simulated$n)
      if(simulated$n < pattern$n){
        p <- spatstat::runifpoint(n=dif, win=pattern$window)
        simulated <- spatstat::superimpose(simulated, p)
      }
      else{
        r <- sample(1:max(simulated$n), size=dif, replace=F)
        simulated <- simulated[-r]
      }
    }
  }
  else{simulated <- spatstat::runifpoint(n=spatstat::unmark(pattern)$n, win=spatstat::unmark(pattern)$window)} # create simulation data

  species <- rep(levels(pattern$marks), spatstat::summary.ppp(pattern)$marks[[1]]) # get marks
  spatstat::marks(simulated) <- factor(species) # assign marks to simulated pattern

  if(pattern$n>=500){ # indirect computation
    pcf_observed <- SHAR::Pcf.Fast(pattern)
    pcf_simulated <- SHAR::Pcf.Fast(simulated)
  }

  else{ # direct computation
    pcf_observed <- spatstat::pcf(pattern, correction="best", divisor="d") # g(r) observed data
    pcf_simulated <- spatstat::pcf(simulated, correction="best", divisor="d") # g(r) simulated data
  }

  gest_observed <- spatstat::Gest(pattern, correction="best") # G(r) observed data
  gest_simulated <- spatstat::Gest(simulated, correction="best") # g(r) simulated pattern

  pcfmulti_observed <- SHAR::Pcf.Multi(pattern, r_max=15, r_length=515) # iSAR observed data
  pcfmulti_simulated <- SHAR::Pcf.Multi(simulated, r_max=15, r_length=515) # iSAR simulated data

  gmulti_observed <- SHAR::Gest.Multi(pattern, r_max=30, r_length=515) # Gmulti(r) observed data
  gmulti_simulated <- SHAR::Gest.Multi(simulated, r_max=30, r_length=515) # Gmulti(r) simulated data

  e0_pcf <- mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm=T) # energy g(r)
  e0_gest <- mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm=T) # energy G(r)
  e0_pcfmulti <- mean(abs(pcfmulti_observed$Mean - pcfmulti_simulated$Mean), na.rm=T) # energy isar
  e0_gmulti <- mean(abs(gmulti_observed$Mean - gmulti_simulated$Mean), na.rm=T) # energy Gmulti

  e0 <- e0_pcf + e0_gest + e0_pcfmulti + e0_gmulti # overall energy

  pb <- utils::txtProgressBar(max=max_runs, style=3)
  for(i in 1:max_runs){ # pattern reconstruction
    relocated <- simulated # create relocation data

    rp <- sample(size=1, 1:relocated$n) # random point of pattern
    relocated_temp <- relocated[-rp] # remove point from pattern
    point <- spatstat::runifpoint(n=1, win=relocated$window) # create random coordinates
    spatstat::marks(point) <- spatstat::marks(relocated[rp]) # assign marks
    relocated <- spatstat::superimpose(relocated_temp, point) # add point to pattern

    if(relocated$n>=500){ # indirect computation
      k_relocated <- spatstat::Kest(relocated, correction="good") # K(r) after relocation
      pcf_relocated <- spatstat::pcf.fv(k_relocated, spar=0.5, method="d") # g(r) after relocation
    }
    else{pcf_relocated <- spatstat::pcf(relocated, correction="best", divisor="d")} # g(r) after relocation

    gest_relocated <- spatstat::Gest(relocated, correction="best") # G(r) after relocation
    pcfmulti_relocated <- SHAR::Pcf.Multi(relocated, r_max=15, r_length=515) # iSAR after relocation
    gmulti_relocated <- SHAR::Gest.Multi(relocated, r_max=30, r_length=515) # Gmulti(r) simulated data

    e_relocated_pcf <- mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm=T) # energy g(r) after relocation
    e_relocated_gest <- mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm=T) # energy G(r) after relocation
    e_relocated_pcfmulti <- mean(abs(pcfmulti_observed$Mean - pcfmulti_relocated$Mean), na.rm=T) # energy iSAR after relocation
    e_relocated_gmulti <- mean(abs(gmulti_observed$Mean - gmulti_relocated$Mean), na.rm=T) # energy Gmulti

    e_relocated <- e_relocated_pcf + e_relocated_gest + e_relocated_pcfmulti + e_relocated_gmulti # total energy after relocation

    if(e_relocated<e0){ # lower energy after relocation
      simulated <- relocated # keep relocated pattern
      e0 <- e_relocated # keep e_relocated
      pcf_simulated <- pcf_relocated # keep pcf_relocated
      gest_simulated <- gest_relocated # keep Gest_relocated
      pcfmulti_simulated <- pcfmulti_relocated # keep iSAR
      gmulti_simulated <- gmulti_relocated # keep Gmulti(r)
    }

    utils::setTxtProgressBar(pb, i)
    if(e0<=e_threshold){break}
  }

  # Writing results on console #
  print(paste0("Remaining energy simultaneously reconstruction: ", round(e0, nchar(e_threshold)-2)))
  cat("\n")

  return(simulated) # return results
}
