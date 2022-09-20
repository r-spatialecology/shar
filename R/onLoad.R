.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["getCRUCLdata"] = "http://packages.ropensci.org"
  options(repos = repos)
  invisible(repos)
}
