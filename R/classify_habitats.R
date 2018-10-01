#' classify_habitats
#'
#' @description Classify habitats
#'
#' @param raster RasterLayer
#' @param classes Number of classes
#' @param style Style of classification. See \code{\link{classIntervals}} for more details
#'
#' @details
#' Classifies a RasterLayer with continious values into n discrete classes
#'
#' @seealso
#' \code{\link{classIntervals}}
#'
#' @return RasterLayer
#'
#' @examples
#' \dontrun{
#' landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, user_seed = 1)
#' landscape_classified <- SHAR::classify_habitats(landscape, classes = 5)
#' }
#'
#' @aliases classify_habitats
#' @rdname classify_habitats
#'
#' @references
#' Jenks, G. F., & Caspall, F. C. (1971). Error in choroplethic maps: Definition,
#' measurement, reduction. Annals of the Association of American Geographers, 61(2), 217–244.

#' @export
classify_habitats <- function(raster, classes = 5, style = "fisher"){

  raster_values <- na.omit(raster::getValues(raster))

  breaks <- classInt::classIntervals(var = raster_values, n = classes, style = style)

  result <- raster::cut(raster, breaks = breaks$brks, include.lowest = TRUE)

  return(result)
}
