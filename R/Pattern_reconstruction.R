#' Pattern reconstruction
#'
#' Pattern reconstruction using simulated annealing
#' @param pattern [\code{ppp(1)}] ppp object of the spatstat package
#' @param method [\code{string(1)}] Method used for the reconstruction. Either "only_spatial", "splitting_species", "random_labelling" or "simultaneously"
#' @param number_reconstructions [\code{numeric(1)}] Number of reconstructed patterns
#' @param max_runs [\code{numeric(1)}] Number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#' @param parallel [\code{logical(1)}]\cr If TRUE, parallel computing with the help of the foreach package is used
#' @param verbose [\code{logical(1)}]\cr If TRUE, progress is printed
#'
#' @return List containing reconstructed patterns and observed pattern

#' @importFrom doRNG %dorng%
#' @importFrom foreach %dopar%
#' @importFrom magrittr %>%

#' @export
Pattern.Reconstruction <- function(pattern, method="only_spatial",
                                   number_reconstructions=1, max_runs=10000, e_threshold=0.01,
                                   fitting=F, verbose=F){

  doFuture::registerDoFuture()
  future::plan(future::multisession)

  if(method=="only_spatial"){
    result <- foreach::foreach(i=1:number_reconstructions)%dorng%{
      SHAR::Spatial.Reconstruction(pattern=pattern, max_runs=max_runs,
                                   e_threshold=e_threshold, fitting=fitting, verbose=verbose)
    }
  }

  else if(method=="splitting_species"){
    result <- foreach::foreach(i=1:number_reconstructions)%dorng%{
      SHAR::Splitting.Reconstruction(pattern=pattern, max_runs=max_runs,
                                     e_threshold=e_threshold, fitting=fitting, verbose=verbose)
    }
  }

  else if(method=="random_labeling"){
    result <- foreach::foreach(i=1:number_reconstructions)%dorng%{
      SHAR:: Labeling.Reconstruction(pattern=pattern, max_runs=max_runs,
                                     e_threshold=e_threshold, fitting=fitting, verbose=verbose)
    }
  }

  else if(method=="simultaneously"){
    result <- foreach::foreach(i=1:number_reconstructions)%dorng%{
      SHAR::Simultaneously.Reconstruction(pattern=pattern, max_runs=max_runs,
                                          e_threshold=e_threshold, fitting=fitting, verbose=verbose)
    }
  }

  else{
    print("Please select either 'only_spatial', 'splitting_species', 'random_labeling' or 'simultaneously' as method")
    result <- list("NA")
  }

  if(method=="only_spatial"){result[[length(result)+1]] <- spatstat::unmark(pattern)}
  else{result[[length(result)+1]] <- pattern}
  names(result) <-  c(rep(paste0("Randomized_", 1:(length(result)-1))), "Observed")
  return(result)
}
