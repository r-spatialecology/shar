## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, message = FALSE--------------------------------------
  library(raster)
  library(spatstat)

## ------------------------------------------------------------------------
  pattern <- spatstat.data::bei
  habitats <- spatstat.data::bei.extra$elev

