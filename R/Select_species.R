#' Internal help function
#'
#' Internal help function to select pattern with only species as mark
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat packages containing multivariate point pattern

#' @export
select_species <- function(pattern) {
  result <- tryCatch(
    {
      spatstat::subset.ppp(pattern, select=Species)
    },
    error=function(cond) {
      return(pattern)
    },
    warning=function(cond) {
      return(pattern)
    }
  )
  return(result)
}


