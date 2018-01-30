#' Multivariate pcf-function
#'
#' The multivariate pair-correlation function pcf(r) to all points belonging to a different species than the observed point
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat packages containing multivariate point pattern
#' @param r_max [\code{numeric(1)}] Maximum distance r of multivariate G function
#' @param r_length [\code{numeric(1)}] Number of evaluated distances r
#'
#' @return Data frame containing r and the multivaria pcf function for all species separately and the overall mean

#' @export
Pcf.Multi <- function(pattern, r_max, r_length=515){

  if(length(names(pattern$marks))>0){pattern<-spatstat::subset.ppp(pattern, select=Species)}

  species <- levels(spatstat::marks(pattern)) # get species
  r <- seq(from=0, to=r_max, length=r_length) # create r-values for Gmulti
  df <- data.frame(r=r) # create dataframe

  for(i in 1:length(species)){ # loop over all species
    p <- spatstat::pcfmulti(pattern, I=spatstat::marks(pattern)==species[i], J=spatstat::marks(pattern)!=species[i], # G_multi(r)
                            r=r, divisor="d", correction="Ripley")
    df <- data.frame(df, p[[3]]) # save into dataframe
    names(df)[i+1] <- species[i] # rename result according to species
  }

  df$Mean <- rowMeans(df[,-1]) # calcluate mean-values

  return(df) # return dataframe as result
}
