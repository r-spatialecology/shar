#' classint_to_vector
#'
#' @description Convert classIntervals to vector
#'
#' @param x classIntervals object
#' @param digits Integer with digits used for rounding.
#'
#' @details
#' Returns a character vector with breaks of a \code{classIntervals} object. If
#' \code{digits = NULL}, results will not be rounded
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' classint_to_vector(x = landscape_classified$breaks, digits = 4)
#' }
#'
#' @aliases classint_to_vector
#' @rdname classint_to_vector
#'
#' @keywords internal
classint_to_vector <- function(x, digits = NULL) {

  # get interval closures
  if (methods::slot(x, "intervalClosure") == "left") {

    brackets <- c("[", ")")

  } else {

    brackets <- c("(", "]")

  }

  # round if digits is present
  if (!is.null(digits)) {

    x$brks <- round(x = x$brks, digits = digits)

  }

  # create vector of breaks
  breaks <- paste(x$brks[1:length(x$brks) - 1], x$brks[2:length(x$brks)], sep = ",")

  # add intervall closures
  breaks <- paste0(brackets[1], breaks, brackets[2])

  return(breaks)

}
