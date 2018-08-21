#' Torus translation
#'
#' Shifts the raster in x or y direction on a tours
#' @param raster [\code{raster(1)}] Raster of the raster package
#' @param x_shift [\code{numeric(1)}] Number of pixels shifted in x direction
#' @param y_shift [\code{numeric(1)}] Number of pixels shifted in y direction
#'
#' @return Raster object of the raster package

#' @export
translate_raster <- function(raster, x_shift=0, y_shift=0){

  matrix_raster <- raster::as.matrix(raster)

  x_shift <- x_shift - (nrow(matrix_raster) * (x_shift%/% nrow(matrix_raster)))
  y_shift <- y_shift - (ncol(matrix_raster) * (y_shift%/% ncol(matrix_raster)))

  if(x_shift==0){matrix_shifted<-matrix_raster}
  else{matrix_shifted <- cbind(matrix_raster[,(x_shift + 1):dim(matrix_raster)[2]], matrix_raster[,1:x_shift])}

  if(y_shift==0){matrix_shifted <- matrix_shifted}
  else{matrix_shifted <- rbind(matrix_shifted[(y_shift + 1):dim(matrix_shifted)[1],], matrix_shifted[1:y_shift,])}

  raster_shifted <- raster::raster(matrix_shifted, xmn=raster::xmin(raster), xmx=raster::xmax(raster),
                                   ymn=raster::ymin(raster), ymx=raster::ymax(raster))
  return(raster_shifted)
}
