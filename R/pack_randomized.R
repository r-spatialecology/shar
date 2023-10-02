#' pack_randomized
#'
#' @description Save randomized raster object
#'
#' @param raster rd_ras object with randomized raster.
#'
#' @details
#' Because of how SpatRaster are saved (need to be packed), this function wraps
#' all raster objects and prepares them for saving first. For further details, see \code{wrap}.
#'
#' @seealso
#' \code{\link{unpack_randomized}}
#' \code{\link{wrap}}
#'
#' @return rd_ras
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
#' landscape_random <- randomize_raster(landscape_classified, n_random = 3)
#' x <- pack_randomized(raster = landscape_random)
#' }
#'
#' @aliases pack_randomized
#' @rdname pack_randomized
#'
#' @export
pack_randomized <- function(raster) {

  # check if observed is present
  # wrap observed raster
  if (inherits(x = raster$observed, what = "SpatRaster")) raster$observed <- terra::wrap(raster$observed)

  # wrap all randomized raster
  raster$randomized <- lapply(X = raster$randomized, FUN = terra::wrap)

  return(raster)

}
