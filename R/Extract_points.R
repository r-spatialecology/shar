#' extract_points
#'
#' @description Extract points
#'
#' @param raster RasterLayer
#' @param pattern Point pattern
#'
#' @details
#' Extract number of pattern within each habitat
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pcf_pattern_random <- estimate_pcf_fast(pattern_random)
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
