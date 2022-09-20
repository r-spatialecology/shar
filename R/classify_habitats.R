#' classify_habitats
#'
#' @description Classify habitats
#'
#' @param raster SpatRaster with continuous environmental values.
#' @param return_breaks Logical if breaks should be returned as well.
#' @param ... Arguments passed on to \code{classIntervals}.
#'
#' @details
#' Classifies a SpatRaster from the \code{raster} packages with continuous
#' values into n discrete classes. The \code{cut} function used to classify the raster,
#' uses \code{include.lowest = TRUE}.
#'
#' For more information about the classification methods, see \code{classIntervals} from
#' the \code{classInt} package and/or the provided References. The help page of \code{classIntervals}
#' also includes further possible arguments to find  breaks (e.g., different styles, number
#' of classes, fixed breaks, etc.).
#'
#' @seealso
#' \code{\link{classIntervals}}
#'
#' @return SpatRaster
#'
#' @examples
#' landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
#'
#' landscape_classified <- classify_habitats(terra::rast(landscape), style = "fixed",
#' fixedBreaks = c(0, 0.25, 0.75, 1.0), return_breaks = TRUE)
#'
#' @aliases classify_habitats
#' @rdname classify_habitats
#'
#' @references
#' Armstrong, M.P., Xiao, N., Bennett, D.A., 2003. Using genetic algorithms to create
#' multicriteria class intervals for choropleth maps. Annals of the Association of
#' American Geographers 93, 595–623. <https://doi.org/10.1111/1467-8306.9303005>
#'
#' Dent, B.D., 1999. Cartography: Thematic map design, 5th ed. WCB/McGraw-Hill, Boston, USA.
#' ISBN 978-0-697-38495-9
#'
#' Fisher, W.D., 1958. On grouping for maximum homogeneity. Journal of the American
#' Statistical Association 53, 789–798. <https://doi.org/10.1080/01621459.1958.10501479>
#'
#' Jenks, G.F., Caspall, F.C., 1971. Error in choroplethic maps: Definition, measurement,
#' reduction. Annals of the Association of American Geographers 61, 217–244.
#' <https://doi.org/10.1111/j.1467-8306.1971.tb00779.x>
#'
#' Jiang, B., 2013. Head/tail breaks: A new classification scheme for data with a
#' heavy-tailed distribution. The Professional Geographer 65, 482-494.
#' <https://doi.org/10.1080/00330124.2012.700499>
#'
#' Slocum, T.A., McMaster, R.B., Kessler, F.C., Howard, H.H., 2009. Thematic cartography
#' and geovisualization, 3rd ed. ed, Prentice Hall Series in Geographic Information Science.
#' Pearson Prentice Hall, Upper Saddle River, USA. ISBN 978-0-13-229834-6
#'
#' Wand, M. P., 1995. Data-based choice of histogram binwidth. The American
#' Statistician 51, 59-64. <https://doi.org/10.1080/00031305.1997.10473591>
#'
#' @export
classify_habitats <- function(raster, return_breaks = FALSE, ...){

  raster_values <- terra::values(raster) # get all values

  breaks <- classInt::classIntervals(var = raster_values, ...) # use classInt to find breaks

  result <- terra::classify(x = raster, rcl = breaks$brks, include.lowest = TRUE)

  terra::values(result) <- terra::values(result) + 1

  names(result) <- "layer"

  # return SpatRaster and breaks
  if (return_breaks) {

    return(list(raster = result, breaks = breaks))

  # return only SpatRaster
  } else {

    return(result)

  }
}
