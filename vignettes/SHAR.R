## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, message = FALSE--------------------------------------
  library(raster)
  library(spatstat)

## ------------------------------------------------------------------------
  pattern <- bei
  habitats <- bei.extra$elev

