#' translate_raster
#'
#' @description Torus translation
#'
#' @param raster RasterLayer.
#' @param return_input The original input data is returned as last list entry

#'
#' @details
#' Torus translation test as described in Harms et al. (20001). The raster is shifted
#' in all four cardinal directions by steps equal to the raste resolution. If a cell
#' exits the extent on one side, it enters the extent on the opposite side.
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
translate_raster <- function(raster, return_input = TRUE){

  # check if dim of raster are equal
  if(!raster::nrow(raster) == raster::ncol(raster)) {
    stop("Torus translation only works for raster with nrow == ncol")
  }

  steps_x <- seq(from = 0, to = raster::nrow(raster), by = 1) # all steps in x-direction

  steps_y <- seq(from = 0, to = raster::ncol(raster), by = 1) # all steps in y-direction

  steps_xy <- expand.grid(x = steps_x, y = steps_y) # grid with all possible x-y combinations

  # remove combinations identical to original raster
  steps_xy <- steps_xy[-c(1, length(steps_x), max(steps_x) * length(steps_y) + 1, length(steps_x)*length(steps_y)),]

  matrix_raster <- raster::as.matrix(raster) # convert to matrix

  # loop through all possible steps
  result <- lapply(seq_len(nrow(steps_xy)), function(current_row){

    x_shift <- steps_xy[current_row, 1] - (nrow(matrix_raster) * (steps_xy[current_row, 1] %/% nrow(matrix_raster)))

    y_shift <- steps_xy[current_row, 2] - (ncol(matrix_raster) * (steps_xy[current_row, 2] %/% ncol(matrix_raster)))

    if(x_shift == 0){matrix_shifted <- matrix_raster}

    else{matrix_shifted <- cbind(matrix_raster[,(x_shift + 1):dim(matrix_raster)[2]], matrix_raster[,1:x_shift])}

    if(y_shift == 0){matrix_shifted <- matrix_shifted}

    else{matrix_shifted <- rbind(matrix_shifted[(y_shift + 1):dim(matrix_shifted)[1],], matrix_shifted[1:y_shift,])}

    # convert back to raster
    raster::raster(matrix_shifted,
                   xmn = raster::xmin(raster), xmx = raster::xmax(raster),
                   ymn = raster::ymin(raster), ymx = raster::ymax(raster))
  })

  # return input raster
  if(return_input){
    result[[length(result) + 1]] <- raster # add input raster as last list entry
    names(result) <- c(paste0("randomized_", 1:(length(result)-1)), "observed") # set names
  }

  else{
    names(result) <- paste0("randomized_", 1:(length(result))) # set names
  }

  return(result)
}
