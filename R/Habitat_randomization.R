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

#' @return Raster object of the raster package with randomized habitat maps

#' @importFrom foreach %dopar%
#' @export
Habitat.Randomization <- function(raster, method='randomization_algorithm', number_maps=1, number_neighbours=8,
                                  parallel=F, verbose=T){

  if(parallel==T){
    if(method=="torus_translation"){
      result <- list()
      print("No support of parallel torus translation at the moment")
      result[[1]] <- "NA"
    }

    else if(method=="randomization_algorithm"){
      if(Sys.info()[[1]]=="Windows" || Sys.info()[[1]]=="Darwin"){cores <- parallel::detectCores()/2}
      else{cores <- parallel::detectCores()}

      cl <- parallel::makeCluster(cores)
      doSNOW::registerDoSNOW(cl)

      if(verbose==T){
        print(paste0("Parallel computation using: ", cores, " cores"))
        pb <- utils::txtProgressBar(max=number_maps, style=3)
        progress <- function(n) utils::setTxtProgressBar(pb, n)
        opts <- list(progress=progress)
      }
      else{opts<-list()}

      result <- foreach::foreach(i=1:number_maps, .options.snow=opts)%dopar%{
        SHAR::Randomization.Algorithm(raster=raster, number_neighbours=number_neighbours)
      }
      if(verbose==T){close(pb)}
      parallel::stopCluster(cl)
    }

    else{
      print("Please select either torus_translation or randomization_algorithm as method")
      result[[1]] <- "NA"
    }
  }


  else{
    if(verbose==T){print("Non-parallel computation")}
    result <- list()

    if(method=="torus_translation"){
      # All steps in x-direction
      steps_x <- seq(from=0, to=raster::nrow(raster), by=1)
      # All steps in y-direction
      steps_y <- seq(from=0, to=raster::ncol(raster), by=1)
      # Grid with all possible x-y combinations
      steps_xy <- expand.grid(x=steps_x, y=steps_y)
      # Remove combinations identical to original raster
      steps_xy <- steps_xy[-c(1, length(steps_x), max(steps_x)*length(steps_y)+1 , length(steps_x)*length(steps_y)),]

      if(verbose==T){pb <- utils::txtProgressBar(max=nrow(steps_xy), style=3)}

      for(i in 1:nrow(steps_xy)){
        result[[length(result)+1]] <- SHAR::Torus.Translation(raster=raster,x_shift=steps_xy[i,1], y_shift=steps_xy[i,2])
        if(verbose==T){utils::setTxtProgressBar(pb, i)}
      }
    }

    else if(method=="randomization_algorithm"){
      if(verbose==T){pb <- utils::txtProgressBar(max=number_maps, style=3)}

      for(i in 1:number_maps){
        result[[i]] <- SHAR::Randomization.Algorithm(raster=raster, number_neighbours=number_neighbours)
        if(verbose==T){utils::setTxtProgressBar(pb, i)}
      }
    }

    else{
      print("Please select either torus_translation or randomization_algorithm as method")
      result[[1]] <- "NA"
    }

    if(verbose==T){close(pb)}
  }

  result[[length(result)+1]] <- raster
  names(result) <-  c(rep(paste0("Randomized_", 1:(length(result)-1))), "Observed")
  return(result)
}
