.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["getCRUCLdata"] = "https://packages.ropensci.org"
  options(repos = repos)
  invisible(repos)
}
