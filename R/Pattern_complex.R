#' Help function
#'
#' Internal help function to create complex pattern
#' @param owin [\code{owin(1)}] Window of the spatstat package
#'
#' @return ppp object of the spatstat package

#' @export
Pattern.Complex <- function(owin){

  s <- sample(x=seq(from=floor(owin$xrange[2]/8), to=floor(owin$xrange[2]/4), by=1), size=1) # Program not fixed!

  x <- seq(from=owin$xrange[1], to=owin$xrange[2], by=s)
  y <- seq(from=owin$yrange[1], to=owin$yrange[2], by=s)

  regular_grid <- expand.grid(x=x,y=y)
  regular_grid$id <- 1:nrow(regular_grid)

  scale_x <- (owin$xrange[2] - owin$xrange[1])*0.025
  scale_y <- (owin$yrange[2] - owin$yrange[1])*0.025

  clustered_df <- data.frame(Cluster=numeric(), Point=numeric(), x=numeric(), y=numeric())
  for(i in 1:nrow(regular_grid)){
    for(j in 1:sample(5:10,1)){
      x_j <- regular_grid$x[i] + stats::runif(1, -scale_x, scale_x)
      y_j <- regular_grid$y[i] + stats::runif(1, -scale_y, scale_y)
      clustered_df[nrow(clustered_df)+1,] <- cbind(i, j, x_j, y_j)
    }
  }

  regular <- spatstat::ppp(x=regular_grid$x, y=regular_grid$y, window=owin)

  clustered_df<- clustered_df[spatstat::inside.owin(clustered_df$x, clustered_df$y, w=owin),]
  clustered <- spatstat::ppp(x=clustered_df$x, y=clustered_df$y, window=owin)

  complex <- spatstat::superimpose(regular, clustered, W=owin)

  return(complex)
}
