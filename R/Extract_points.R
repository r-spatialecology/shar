#' Extract points within habitat
#'
#' Internal help function extracts the number of counts within each habitat
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package.
#' @param points [\code{spatial points(1)}]\cr Spatial points of the sp package
#' @param method [\code{string(1)}]\cr Method which was used to produce results
#' @return Table with counts in each habitat

#' @export
extract_points <- function(raster, points, method){
  if(method == 'random_raster'){
    raster %>%
      raster::extract(y=points) %>%
      factor(levels=1:5) %>%
      table() %>%
      tibble::as.tibble() %>%
      setNames(c("Habitat", 'Count')) %>%
      dplyr::mutate(Habitat = as.integer(Habitat))
  }

  else if(method == 'random_pattern'){
    points %>%
      spatstat::coords() %>%
      sp::SpatialPoints() %>%
      raster::extract(raster, ., factor=T) %>%
      factor(levels=1:5) %>%
      table() %>%
      tibble::as.tibble() %>%
      setNames(c("Habitat", 'Count')) %>%
      dplyr::mutate(Habitat = as.integer(Habitat))
    }

  else{stop('Please select valid metho')}
}
