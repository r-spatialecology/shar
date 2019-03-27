#' classify_habitats
#'
#' @description Classify habitats
#'
#' @param raster RasterLayer.
#' @param classes Number of classes.
#' @param style Style of classification.
#'
#' @details
#' Classifies a RasterLayer with continuous values into n discrete classes. Consequently,
#' classes are non-overlapping (and left-closed). For more information see `classIntervals`.
#'
#' @seealso
#' \code{\link{classIntervals}}
#'
#' @return RasterLayer
#'
#' @examples
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#'
#' @aliases classify_habitats
#' @rdname classify_habitats
#'
#' @references
#' Armstrong, M. P., Xiao, N., Bennett, D. A., 2003. "Using genetic algorithms
#' to create multicriteria class intervals for choropleth maps". Annals,
#' Association of American Geographers, 93 (3), 595-623
#'
#' Jenks, G. F., Caspall, F. C., 1971. "Error on choroplethic maps: definition,
#' measurement, reduction". Annals, Association of American Geographers, 61 (2), 217-244
#'
#' Dent, B. D., 1999, Cartography: thematic map design. McGraw-Hill, Boston, 417 pp.
#
#' Slocum TA, McMaster RB, Kessler FC, Howard HH 2005 Thematic Cartography and
#' Geographic Visualization, Prentice Hall, Upper Saddle River NJ.
#'
#' Fisher, W. D. 1958 "On grouping for maximum homogeneity", Journal of the American
#' Statistical Association, 53, 789-798

#' @export
classify_habitats <- function(raster, classes = 5, style = "fisher"){

  raster_values <- raster::values(raster) # get all values

  breaks <- classInt::classIntervals(var = raster_values,
                                     n = classes, style = style) # get class intervals

  result <- raster::cut(raster, breaks = breaks$brks, include.lowest = TRUE) # classify raster

  return(result)
}
