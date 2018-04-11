#' Detection of habitat association
#'
#' Correct and false detections of the results of habitat associations
#' @param input [\code{list(1)}]\cr List created with Results.Habitat.Associations
#'
#' @return Tibble with correct and false detections

#' @export
Detection.Habitat.Association <- function(input){

  habitats <- input %>%
    names() %>%
    stringr::str_sub(-1) %>%
    as.numeric()

  associations <- input %>%
    names() %>%
    stringr::str_extract( "(?<=_).+?(?=_)")

  result <- purrr::pmap_dfr(list(input, habitats, associations), function(x, y, z){
    tibble::as.tibble(cbind(Correct=sum(x$Significance[x$Habitat==y] == z, na.rm=T),
                            False=sum(x$Significance[x$Habitat!=y] == z, na.rm=T)))
  }, .id = 'Species')
  return(result)
}
