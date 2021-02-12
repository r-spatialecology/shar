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
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#' extract_points(raster = landscape_classified, pattern = species_b)
#'
#' @aliases extract_points
#' @rdname extract_points

#' @export
extract_points <- function(raster, pattern){

  habitat_levels <- sort(unique(raster::values(raster))) # get all habitats sorted

  pattern <- spatstat.geom::coords(pattern) # extract only coords of points

  # get habitat points are located within
  pattern_extracted <- factor(raster::extract(x = raster,
                                              y = pattern,
                                              factor = TRUE),
                              levels = habitat_levels)

  result <- utils::stack(table(pattern_extracted)) # count number of points within each habitat

  names(result) <- c("count", "habitat") # rename df

  return(result)
}
