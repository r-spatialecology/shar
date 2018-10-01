#' translate_raster
#'
#' @description Torus translation
#'
#' @param raster RasterLayer or list of RasterLayers
#'
#' @details
#' Torus translation
#'
#' @seealso
#' \code{\link{randomization_algorithm}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, user_seed = 1)
#' landscape_classified <- SHAR::classify_habitats(landscape, classes = 5)
#' landscape_random <- translate_raster(landscape_classified)
#' }
#'
#' @aliases translate_raster
#' @rdname translate_raster
#'
#' @references
#' Harms, K. E., Condit, R., Hubbell, S. P., & Foster, R. B. (2001). Habitat associations
#' of trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology, 89(6), 947–959.

#' @export
translate_raster <- function(raster){

  # result <- list()

  steps_x <- seq(from = 0, to=raster::nrow(raster), by = 1) # All steps in x-direction
  steps_y <- seq(from = 0, to=raster::ncol(raster), by = 1) # All steps in y-direction

  steps_xy <- expand.grid(x = steps_x, y = steps_y) # Grid with all possible x-y combinations
  steps_xy <- steps_xy[-c(1, length(steps_x), max(steps_x) * length(steps_y) + 1, length(steps_x)*length(steps_y)),] # Remove combinations identical to original raster

  matrix_raster <- raster::as.matrix(raster)

  result <- lapply(1:nrow(steps_xy), function(current_row){
    x_shift <- steps_xy[current_row, 1] - (nrow(matrix_raster) * (steps_xy[current_row, 1] %/% nrow(matrix_raster)))
    y_shift <- steps_xy[current_row, 2] - (ncol(matrix_raster) * (steps_xy[current_row, 2] %/% ncol(matrix_raster)))

    if(x_shift == 0){matrix_shifted <- matrix_raster}
    else{matrix_shifted <- cbind(matrix_raster[,(x_shift + 1):dim(matrix_raster)[2]], matrix_raster[,1:x_shift])}

    if(y_shift == 0){matrix_shifted <- matrix_shifted}
    else{matrix_shifted <- rbind(matrix_shifted[(y_shift + 1):dim(matrix_shifted)[1],], matrix_shifted[1:y_shift,])}

    raster_shifted <- raster::raster(matrix_shifted, xmn=raster::xmin(raster), xmx=raster::xmax(raster),
                                     ymn=raster::ymin(raster), ymx=raster::ymax(raster))
    return(raster_shifted)
  })

  result[[length(result) + 1]] <- raster
  names(result) <-  c(rep(paste0("randomized_", 1:(length(result)-1))), "observed")

  return(result)
}
