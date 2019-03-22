#' @title shar
#'
#' @description
#' Analyse species-habitat associations in R. Therefore, information about the
#' location of the species is needed and about the environmental conditions. To test
#' for significance habitat associations, one of the two components is randomized.
#' Methods are mainly based on Plotkin et al. (2000) <doi:10.1006/jtbi.2000.2158> and
#' Harms et al. (2001) <doi:10.1111/j.1365-2745.2001.00615.x>.
#'
#' @name shar
#' @docType package
#' @useDynLib shar, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

# Global variables
globalVariables(c("count",
                  "habitat",
                  "hi",
                  "iso",
                  "lo",
                  "r",
                  "summary_function",
                  "theo",
                  "type",
                  "x_r"))



