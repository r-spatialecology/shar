#' Habitat randomization
#'
#' Randomization of habitat maps using methods proposed by Harms et al. (2001)
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package with discrete habitats
#' @param method [\code{string(1)}]\cr String choosing the method. Either "Torus_translation" or "Randomization_algorithm"
#' @param number_maps [\code{numeric(1)}]\cr Number of created habitat maps for method="Randomization_algorithm"
#' @param number_neighbours [\code{numeric(1)}]\cr Number of neighbours. See raster::adjacent(direction=number_neighbours) for more details
#' @param parallel [\code{logical(1)}]\cr If TRUE, parallel computing with the help of the foreach package is used
#' @param verbose [\code{logical(1)}]\cr If TRUE, progress is printed
#' @references Harms, K. E., Condit, R., Hubbell, S. P., & Foster, R. B. (2001).
#' Habitat associations of trees and shrubs in a 50-ha neotropical forest plot.
#' Journal of Ecology, 89(6), 947–959.
#'
#' @return Raster object of the raster package with randomized habitat maps

#' @importFrom doRNG %dorng%
#' @importFrom foreach %dopar%
#' @importFrom magrittr %>%

#' @export
Habitat.Randomization <- function(raster, method='randomization_algorithm', number_maps=1, number_neighbours=8){

  if(method=="torus_translation"){
    result <- list()

    steps_x <- seq(from=0, to=raster::nrow(raster), by=1) # All steps in x-direction
    steps_y <- seq(from=0, to=raster::ncol(raster), by=1) # All steps in y-direction

    steps_xy <- expand.grid(x=steps_x, y=steps_y) # Grid with all possible x-y combinations
    steps_xy <- steps_xy[-c(1, length(steps_x), max(steps_x)*length(steps_y)+1, length(steps_x)*length(steps_y)),] # Remove combinations identical to original raster

    for(i in 1:nrow(steps_xy)){
      result[[length(result)+1]] <- SHAR::Torus.Translation(raster=raster,x_shift=steps_xy[i,1], y_shift=steps_xy[i,2])
    }
  }

  else if(method=="randomization_algorithm"){
    doFuture::registerDoFuture()
    future::plan(multisession)

    result <- foreach::foreach(i=1:number_maps)%dorng%{
      SHAR::Randomization.Algorithm(raster=raster, number_neighbours=number_neighbours)
    }
  }

  else{
    print("Please select either torus_translation or randomization_algorithm as method")
    result <- list("NA")
  }

  result[[length(result)+1]] <- raster
  names(result) <-  c(rep(paste0("Randomized_", 1:(length(result)-1))), "Observed")
  return(result)
}
