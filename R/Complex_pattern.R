#' Help function
#'
#' Internal help function to create complex pattern
#' #' @param owin [\code{owin(1)}] Window of the spatstat package
#'
#' #' @return ppp object of the spatstat package

#' @export
Complex.Pattern <- function(owin){
  s <- sample(x=75:100, size=1) # Program not fixed!
  x <- seq(owin$xrange[1], owin$xrange[2], s)
  y <- seq(owin$yrange[1], owin$yrange[2], s)

  regular_grid <- expand.grid(x=x,y=y)
  regular_grid$id <- 1:nrow(regular_grid)

  clustered <- data.frame(Cluster=numeric(), Point=numeric(), x=numeric(), y=numeric())
  for(i in 1:nrow(regular_grid)){
    for(j in 1:sample(5:25,1)){
      x_j <- regular_grid$x[i] + runif(1, -25, 25)
      y_j <- regular_grid$y[i] + runif(1, -25, 25)
      clustered[nrow(clustered)+1,] <- cbind(i, j, x_j, y_j)
    }
  }

  capture.output(regular <- spatstat::ppp(x=regular_grid$x, y=regular_grid$y, window=owin))
  capture.output(clustered <- spatstat::ppp(x=clustered$x, y=clustered$y, window=owin))
  capture.output(complex <- spatstat::superimpose(regular, clustered, W=owin))

  return(complex)
}
