# Source: https://github.com/geanders/hurricaneexposure/blob/master/R/zzz.R

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  check_pkgs <- requireNamespace("getCRUCLdata", quietly = TRUE)

  .pkgenv[["check_pkgs"]] <- check_pkgs

}

.onAttach <- function(libname, pkgname) {

  if (!.pkgenv$check_pkgs) {

    message <- paste("The vignettes of this package require the 'getCRUCLdata' package",
                     "which is not available on CRAN anymore. To install the package, please",
                     "run 'install.packages('getCRUCLdata', repos = 'http://packages.ropensci.org', typ = 'source')'.")

    message <- paste(strwrap(message), collapse = "\n")

    packageStartupMessage(message)
  }
}
