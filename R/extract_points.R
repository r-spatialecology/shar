#' extract_points
#'
#' @description Extract points
#'
#' @param raster RasterLayer with environmental data
#' @param pattern ppp object with point pattern.
#'
#' @details
#' The function extracts the number of points within each discrete habitat.
#'
#' @seealso
#' \code{\link{results_habitat_association}}
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(landscape, n = 5, style = "fisher")
#' extract_points(raster = landscape_classified, pattern = species_b)
#' }
#'
#' @aliases extract_points
#' @rdname extract_points
#'
#' @keywords internal
extract_points <- function(raster, pattern){

  habitat_levels <- sort(unique(raster::values(raster))) # get all habitats sorted

  pattern <- spatstat.geom::coords(pattern) # extract only coords of points

  # get habitat points are located within
  pattern_extracted <- factor(raster::extract(x = raster, y = pattern, factor = TRUE),
                              levels = habitat_levels)

  result <- utils::stack(table(pattern_extracted)) # count number of points within each habitat

  names(result) <- c("count", "habitat") # rename df

  return(result)
}
