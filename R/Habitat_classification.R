#' Habitat classification
#'
#' Classify continuous habitats values into discrete habitat types
#' @param raster [\code{raster(1)}]\cr raster object of the raster package with continiuous habitats
#' @param classes [\code{numeric(1)}]\cr Number of classes
#' @param method [\code{string(1)}]\cr Style of breaks (see classInt::classInvervals() for more details)
#' @return Raster object of the raster package with classified habitats

#' @export
Habitat.Classification <- function(raster, classes=5, style='fisher'){
  values <- na.omit(raster::getValues(raster))
  breaks <- classInt::classIntervals(values, n=classes, style=style)

  classification <-  raster::cut(raster, breaks=breaks$brks, include.lowest=T)
  return(classification)
}
