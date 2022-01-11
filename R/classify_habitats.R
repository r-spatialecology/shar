#' classify_habitats
#'
#' @description Classify habitats
#'
#' @param raster RasterLayer with continuous environmental values.
#' @param classes Integer with number of classes or vector of class breaks.
#' @param style Character with method of classification. See Details section for
#' more information.
#' @param return_classes Logical if breaks should be returned as well.
#' @param ... Arguments passed on to \code{cut}.
#'
#' @details
#' Classifies a RasterLayer from the \code{raster} packages with continuous
#' values into n discrete classes. The \code{cut} function used to classify the raster,
#' uses \code{include.lowest = TRUE}.
#'
#' If \code{classes} is a single integer, breaks are determined using the \code{classInt} package.
#' For more information about the classification method, see \code{\link{classIntervals}} from
#' the \code{classInt} package and/or the provided References.
#'
#' If \code{classes}  is a numerical vector, the classes are used directly as \code{breaks}
#' argument of the \code{cut} function.
#'
#' @seealso
#' \code{\link{classIntervals}}
#'
#' @return RasterLayer
#'
#' @examples
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#' landscape_classified <- classify_habitats(landscape, classes = c(0, 0.25, 0.75, 1.0),
#' return_classes = TRUE)
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
#' Slocum, T.A., McMaster, R.B., Kessler, F.C., Howard, H.H., 2009. Thematic cartography
#' and geovisualization, 3rd ed. ed, Prentice Hall Series in Geographic Information Science.
#' Pearson Prentice Hall, Upper Saddle River, USA. ISBN 978-0-13-229834-6
#'
#' @export
classify_habitats <- function(raster, classes = 5, style = "fisher", return_classes = FALSE,
                              ...){

  raster_values <- raster::values(raster) # get all values

  # number of classes provided; use classIntervals to find breaks
  if (length(classes) == 1) {

    classes <- classInt::classIntervals(var = raster_values,
                                       n = classes, style = style) # get class intervals

    classes <- classes$brks

  }

  result <- raster::cut(raster, breaks = classes, include.lowest = TRUE, ...) # classify raster

  if (return_classes) {

    return(list(raster = result, classes = classes))

  # return only RasterLayer
  } else {

    return(result)

  }
}
