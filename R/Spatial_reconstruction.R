#' Spatial reconstruction
#'
#' Univariate pattern reconstruction of only the spatial structure
#' @param pattern [\code{ppp(1)}] Multivariate ppp object of the spatstat package
#' @param max_runs [\code{numeric(1)}] number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#' @param verbose [\code{logical(1)}]\cr If TRUE, progress is printed
#'
#' @return ppp object of the spatstat package with reconstructed pattern

#' @export
Spatial.Reconstruction <- function(pattern, max_runs=10000, e_threshold=0.01, fitting=F, verbose=T){
  pattern <- spatstat::unmark(pattern) # only spatial points

  if(fitting==T){
    fit_mat <- spatstat::kppm(pattern, clusters="MatClust", statistic="pcf")
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
  else{simulated <- spatstat::runifpoint(n=pattern$n, win=pattern$window)} # create simulation data

  if(pattern$n>=500){ # indirect computation
    pcf_observed <- SHAR::Pcf.Fast(pattern)
    pcf_simulated <- SHAR::Pcf.Fast(simulated)
  }
  else{ # direct computation summary functions
    pcf_observed <- spatstat::pcf(pattern, correction="best", divisor="d") # g(r) observed data
    pcf_simulated <- spatstat::pcf(simulated, correction="best", divisor="d") # g(r) simulated data
  }

  gest_observed <- spatstat::Gest(pattern, correction="best") # G(r) observed data
  gest_simulated <- spatstat::Gest(simulated, correction="best") # G(r) simulated data

  e0_pcf <- mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm=T) # energy g(r)
  e0_gest <- mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm=T) # energy G(r)

  e0 <- e0_pcf + e0_gest # total energy

  if(verbose==T){pb <- utils::txtProgressBar(max=max_runs, style=3)}

  for(i in 1:max_runs){ # pattern reconstruction algorithm
    relocated <- simulated # data for relocation

    rp <- sample(size=1, 1:relocated$n) # random point of pattern
    relocated_temp <- relocated[-rp] # remove point from pattern
    point <- spatstat::runifpoint(n=1, win=relocated$window) # create random coordinates
    relocated <- spatstat::superimpose(relocated_temp, point) # add point to pattern

    if(relocated$n>=500){ # indirect computation of summary functions
      k_relocated <- spatstat::Kest(relocated, correction="good") # K(r) after relocation
      pcf_relocated <- spatstat::pcf.fv(k_relocated, spar=0.5, method="d") # g(r) after relocation
    }
    else{pcf_relocated <- spatstat::pcf(relocated, correction="best", divisor="d")} # direct computation g(r) after relocation

    gest_relocated <- spatstat::Gest(relocated, correction="best") # G(r) after relocation

    e_relocated_pcf <- mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm=T) # energy after relocation
    e_relocated_gest <- mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm=T) # energy after relocation

    e_relocated <- e_relocated_pcf + e_relocated_gest # total energy after relocation

    if(e_relocated<e0){ # lower energy after relocation
      simulated <- relocated # keep relocated pattern
      e0 <- e_relocated # keep e_relocated as e0
      pcf_simulated <- pcf_relocated # keep pcf_relocated
      gest_simulated <- gest_relocated # keep gest_relocated
    }

    if(verbose==T){utils::setTxtProgressBar(pb, i)}
    if(e0<=e_threshold){break} # exit loop
  }

  if(verbose==T){
    close(pb)
    print(paste0("Remaining energy only spatial reconstruction: ", round(e0, nchar(e_threshold)-2)))
    cat("\n")
  }

  return(simulated) # return results
}
