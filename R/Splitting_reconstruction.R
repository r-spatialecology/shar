#' Pattern reconstruction
#'
#' Multivariate pattern reconstruction of every single species indiviually
#' @param pattern [\code{ppp(1)}] Multivariate ppp object of the spatstat package
#' @param max_runs [\code{numeric(1)}] number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#' @param verbose [\code{logical(1)}]\cr If TRUE, progress is printed
#'
#' @return ppp object of the spatstat package with reconstructed pattern

#' @export
Splitting.Reconstruction <- function(pattern, max_runs=10000, e_threshold=0.01, fitting=F, verbose=T){

  pattern <- spatstat::subset.ppp(pattern, select=Species, drop=T) # only species as mark
  n_spec <- length(levels(pattern$marks)) # number of species
  names_spec <- levels(pattern$marks) # names of species

  simulated_final <- spatstat::ppp(window=pattern$window, marks=factor()) # create pattern for overall data

  for(i in 1:n_spec){ # loop for each species
    if(verbose==T){print(paste(names_spec[i], ": ", i , " from ", n_spec, " species reconstruced", sep=""))}

    pattern_species <- pattern[pattern$marks==names_spec[i]] # dataset with current, single species

    reconstructed_species <- SHAR::Spatial.Reconstruction(pattern=pattern_species,
                                                         max_runs=max_runs,
                                                         e_threshold=e_threshold,
                                                         fitting=fitting, verbose=verbose)


    spatstat::marks(reconstructed_species) <- factor(names_spec[i]) # assign marks to reconstruced data

    simulated_final <- spatstat::superimpose(simulated_final, reconstructed_species) # superimpose pattern single species
  }

  if(pattern$n>=500){ # indirect computation overall data
    pcf_observed_final <- SHAR::Pcf.Fast(pattern)
    pcf_simulated_final <- SHAR::Pcf.Fast(simulated_final)
  }

  else{ # direct computation overall data
    pcf_observed_final <- spatstat::pcf(pattern, correction="best", divisor="d") # g(r) observed data
    pcf_simulated_final <- spatstat::pcf(simulated_final, correction="best", divisor="d") # g(r) simulated data
  }

  gest_observed_final <- spatstat::Gest(pattern, correction="best") # G(r) observed data
  gest_simulated_final <- spatstat::Gest(simulated_final, correction="best") # G(r) simulated data

  pcfmulti_observed_final <- SHAR::Pcf.Multi(pattern, r_max=15, r_length=515) # observed iSAR
  pcfmulti_simulated_final <- SHAR::Pcf.Multi(simulated_final, r_max=15, r_length=515) # simulated iSAR

  gmulti_observed_final <- SHAR::Gest.Multi(pattern, r_max=30, r_length=515) # observed Gmulti(r)
  gmulti_simulated_final <- SHAR::Gest.Multi(simulated_final, r_max=30, r_length=515) # simulated Gmulti(r)

  e_final_pcf <- mean(abs(pcf_observed_final[[3]] - pcf_simulated_final[[3]]), na.rm=T) # energy g(r)
  e_final_gest <- mean(abs(gest_observed_final[[3]] - gest_simulated_final[[3]]), na.rm=T) # energy G(r)
  e_final_gmulti <- mean(abs(gmulti_observed_final$Mean - gmulti_simulated_final$Mean), na.rm=T) # energy Gmulti(r)
  e_final_pcfmulti <- mean(abs(pcfmulti_observed_final$Mean - pcfmulti_simulated_final$Mean), na.rm=T) # energy iSAR

  e_final_total <- e_final_pcf + e_final_gest + e_final_gmulti + e_final_pcfmulti # total energy

  if(verbose==T){
    print(paste0("Remaining energy single species reconstruction: ", round(e_final_total, nchar(e_threshold)-2)))
    cat("\n")
  }

  return(simulated_final) # return results
}
