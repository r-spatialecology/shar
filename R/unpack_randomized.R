#' unpack_randomized
#'
#' @description Load randomized raster object
#'
#' @param raster rd_ras object with randomized raster.
#'
#' @details
#' Because of how SpatRaster are saved (need to be packed), this function allows to
#' unpack previously packed raster objects that were saved using \code{pack_randomized}.
#' For further details, see \code{wrap}.
#'
#' @seealso
#' \code{\link{pack_randomized}}
#' \code{\link[terra]{wrap}}
#'
#' @return rd_ras
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
#' landscape_random <- randomize_raster(landscape_classified, n_random = 3)
#' x <- pack_randomized(raster = landscape_random)
#' y <- unpack_randomized(raster = y)
#' }
#'
#' @export
unpack_randomized <- function(raster) {

  # check if observed is present
  # unwrap observed raster
  if (inherits(x = raster$observed, what = "PackedSpatRaster")) raster$observed <- terra::unwrap(raster$observed)

  # wrap all randomized raster
  raster$randomized <- lapply(X = raster$randomized, FUN = terra::unwrap)

  return(raster)

}
