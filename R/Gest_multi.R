#' Multivariate G-function
#'
#' The multivariate nearest neighbour distribution function G(r) to all points belonging to a different species than the observed point
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat packages containing multivariate point pattern
#' @return Data frame containing r and the multivaria G function for all species separately and the overall mean

#' @export
Gest.Multi <- function(pattern){

  species <- pattern %>%
    SHAR::Select.Species() %>%
    spatstat::marks() %>%
    levels()

  r_max <- spatstat::rmax.rule(fun="G", W=pattern$window, lambda=spatstat::intensity(pattern))
  r <- seq(from=0, to=r_max, length=513) # create r-values for Gmulti

  result <- tibble::tibble(r=r)

  for(i in 1:length(species)){ # loop over all species
    result <- pattern %>%
      spatstat::Gmulti(I=spatstat::marks(.)==species[i],
                       J=spatstat::marks(.)!=species[i],
                       r=r, correction="best") %>%
      tibble::as.tibble() %>%
      dplyr::select(rs) %>%
      dplyr::bind_cols(result,.)
  }

  names(result)[-1] <- species
  result$Mean <- rowMeans(result)

  return(result) # return dataframe as result
}
