#' Pattern reconstruction
#'
#' Pattern reconstruction using simulated annealing
#' @param pattern [\code{ppp(1)}] ppp object of the spatstat package
#' @param method [\code{string(1)}] Method used for the reconstruction. Either "only_spatial", "splitting_species", "random_labelling" or "simultaneously"
#' @param number_reconstructions [\code{numeric(1)}] Number of reconstructed patterns
#' @param max_runs [\code{numeric(1)}] Number of maximum iterations
#' @param e_threshold [\code{numeric(1)}] Threshold for energy to reach during reconstruction
#' @param fitting [\code{logical(1)}] If TRUE, a clustered pattern is fitted to the original pattern as starting point
#'
#' @return List containing reconstructed patterns and observed pattern

#' @export
reconstruct_pattern <- function(pattern, method = 'only_spatial',
                                number_reconstructions = 1, max_runs = 10000, e_threshold = 0.01,
                                fitting = FALSE){

  if(method == 'only_spatial'){
    result <- 1:number_reconstructions %>%
      purrr::map(function(x){
        SHAR::reconstruct_spatial(pattern = pattern, max_runs = max_runs,
                                     e_threshold = e_threshold, fitting = fitting)
      })
  }

  else if(method == 'splitting_species'){
    result <- 1:number_reconstructions %>%
      purrr::map(function(x){
        SHAR::reconstruct_splitting(pattern = pattern, max_runs = max_runs,
                                       e_threshold = e_threshold, fitting = fitting)
      })
  }

  else if(method == 'random_labeling'){
    result <- 1:number_reconstructions %>%
      purrr::map(function(x){
        SHAR::reconstruct_labeling(pattern = pattern, max_runs = max_runs,
                                   e_threshold = e_threshold, fitting = fitting)
      })
  }

  else if(method == 'simultaneously'){
    result <- 1:number_reconstructions %>%
      purrr::map(function(x){
        SHAR::reconstruct_simultaneously(pattern = pattern, max_runs = max_runs,
                                         e_threshold = e_threshold, fitting = fitting)
      })
  }

  else{
    print('Please select either "only_spatial", "splitting_species", "random_labeling" or "simultaneously" as method')
    result <- list('NA')
  }

  if(method == 'only_spatial'){result[[length(result) + 1]] <- spatstat::unmark(pattern)}
  else{result[[length(result) + 1]] <- pattern}
  names(result) <-  c(rep(paste0('Randomized_', 1:(length(result)-1))), 'Observed')
  return(result)
}
