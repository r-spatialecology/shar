#' extract_points
#'
#' @description Extract points
#'
#' @param raster RasterLayer.
#' @param pattern Point pattern.
#'
#' @details
#' The function extracts the number of points within each habitat.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, user_seed = 1)
#' landscape_classified <- SHAR::classify_habitats(landscape, classes = 5)
#' species_1 <- spatstat::runifpoint(n = 50, win = spatstat::owin(c(0, 50), c(0, 50)))
#' extract_points(raster = landscape_classified, pattern = species_1)
#' }
#'
#' @aliases extract_points
#' @rdname extract_points

#' @export
extract_points <- function(raster, pattern){

  habitat_levels <- sort(unique(raster::values(raster)))

  pattern <- sp::SpatialPoints(spatstat::coords(pattern))

  pattern_extracted <- factor(raster::extract(x = raster,
                                             y = pattern,
                                             factor = TRUE),
                             levels = habitat_levels)

  result <- utils::stack(table(pattern_extracted))

  names(result) <- c("count", "habitat")

  return(result)
}
