#' Pattern reconstruction
#'
#' Multivariate pattern reconstruction of every single species indiviually
#' @param pattern [\code{ppp(1)}] Multivariate ppp object of the spatstat package
#' @param max_runs [\code{numeric(1)}] number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#'
#' @return ppp object of the spatstat package with reconstructed pattern

#' @export
reconstruct_splitting <- function(pattern, max_runs = 10000, e_threshold = 0.01, fitting = FALSE){

  pattern <- spatstat::subset.ppp(pattern, select = Species, drop = TRUE) # only species as mark
  n_spec <- length(levels(pattern$marks)) # number of species
  names_spec <- levels(pattern$marks) # names of species

  simulated_final <- spatstat::ppp(window = pattern$window, marks = factor()) # create pattern for overall data

  for(i in 1:n_spec){ # loop for each species
    pattern_species <- pattern[pattern$marks == names_spec[i]] # dataset with current, single species

    reconstructed_species <- SHAR::reconstruct_spatial(pattern = pattern_species,
                                                       max_runs = max_runs,
                                                       e_threshold = e_threshold,
                                                       fitting = fitting)


    spatstat::marks(reconstructed_species) <- factor(names_spec[i]) # assign marks to reconstruced data

    simulated_final <- spatstat::superimpose(simulated_final, reconstructed_species) # superimpose pattern single species
  }

  return(simulated_final) # return results
}
