#' Pattern reconstruction
#'
#' Multivariate pattern reconstruction using a random marking approach
#' @param pattern [\code{ppp(1)}] Multivariate ppp object of the spatstat package
#' @param max_runs [\code{numeric(1)}] number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#'
#' @return ppp object of the spatstat package with reconstructed pattern

#' @export
Labeling.Reconstruction <- function(pattern, max_runs, e_threshold, fitting){

  simulated <- SHAR::Spatial.Reconstruction(pattern=pattern, max_runs=max_runs,
                                                  e_threshold=e_threshold,
                                                  fitting=fitting)

  pattern <- spatstat::subset.ppp(pattern, select=Species) # get data with only species as marks

  species <- rep(levels(pattern$marks), spatstat::summary.ppp(pattern)$marks[[1]]) # create marks
  spatstat::marks(simulated) <- factor(species) # assign marks to data

  pcfmulti_observed <- SHAR::Pcf.Multi(pattern, r_max=15, r_length=515) # iSAR observed data
  pcfmulti_simulated <- SHAR::Pcf.Multi(simulated, r_max=15, r_length=515)  # iSAR simulated data

  gmulti_observed <- SHAR::Gest.Multi(pattern, r_max=30, r_length=515) # Gmulti(r) observed data
  gmulti_simulated <- SHAR::Gest.Multi(simulated, r_max=30, r_length=515) # Gmulti(r) simulated data

  e0_pcfmulti <- mean(abs(pcfmulti_observed$Mean - pcfmulti_simulated$Mean), na.rm=T) # energy iSAR
  e0_gmulti <- mean(abs(gmulti_observed$Mean - gmulti_simulated$Mean), na.rm=T) # energy Gmulti

  e0_spec <- e0_pcfmulti + e0_gmulti # overall energy

  pb <- utils::txtProgressBar(max=max_runs, style=3)

  for(i in 1:max_runs){ # mark reconstruction
    relocated <- simulated # create relocated data

    sample_species <- sample(unique(species), size=2)

    point1 <- sample(size=1, 1:relocated[relocated$marks==sample_species[1]]$n) # random point 1
    point2 <- sample(size=1, 1:relocated[relocated$marks==sample_species[2]]$n) # random point 2 (datset without spec1)

    spatstat::marks(relocated[relocated$marks==sample_species[1]][point1]) <- sample_species[2] # swap species
    spatstat::marks(relocated[relocated$marks==sample_species[2]][point2]) <- sample_species[1] # swap species

    pcfmulti_relocated <- SHAR::Pcf.Multi(relocated, r_max=15, r_length=515) # iSAR after relocation
    gmulti_relocated <- SHAR::Gest.Multi(relocated, r_max=30, r_length=515) # Gmulti(r) after relocation

    e_relocated_pcfmulti <- mean(abs(pcfmulti_observed$Mean - pcfmulti_relocated$Mean), na.rm=T) # energy iSAR after relocation
    e_relocated_gmulti <- mean(abs(gmulti_observed$Mean - gmulti_relocated$Mean), na.rm=T) # energy Gmulti(r) after relocation

    e_relocated_spec <- e_relocated_pcfmulti + e_relocated_gmulti # overall energy after relocation

    if(e_relocated_spec<e0_spec){ # lower energy after relocation
      simulated <- relocated # keep relocated data
      e0_spec <- e_relocated_spec # keep energy
      pcfmulti_simulated <- pcfmulti_relocated # keep iSAR
      gmulti_simulated <- gmulti_relocated # keep Gmulti(r)
    }

    utils::setTxtProgressBar(pb, i)

    if(e0_spec<=e_threshold){break} # exit loop
  }

  print(paste0("Remaining energy random labeling reconstuction: ", round(e0_spec, nchar(e_threshold)-2)))
  cat("\n")

  return(simulated) # return results
}
