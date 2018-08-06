#' Habitat classification
#'
#' Classify continuous habitats values into discrete habitat types
#' @param raster [\code{raster(1)}]\cr raster object of the raster package with continiuous habitats
#' @param classes [\code{numeric(1)}]\cr Number of classes
#' @param style [\code{string(1)}]\cr Style of breaks (see classInt::classInvervals() for more details)
#' @return Raster object of the raster package with classified habitats

#' @export
classify_habitats <- function(raster, classes=5, style='fisher'){

  breaks <- raster %>%
    raster::getValues() %>%
    na.omit() %>%
    classInt::classIntervals(n=classes, style=style)

  classification <-  raster::cut(raster, breaks=breaks$brks, include.lowest=T)

  return(classification)
}