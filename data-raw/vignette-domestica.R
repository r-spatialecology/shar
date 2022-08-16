library(dplyr) # For data wrangling
library(rgbif) # For retrieving species occurrence data
library(sf) # For spatial data operations

#### gbif ####

# Retrieve key for Cormus domestica
key <- rgbif::name_backbone(name = 'Cormus domestica', kingdom = 'plants')

# Establish region of interest
roi <- c(xmin = -20, xmax = 45, ymin = 30, ymax = 73)
roi_bbox <- sf::st_bbox(roi, crs = sf::st_crs("EPSG:4326"))
roi_sfc <- sf::st_sfc(sf::st_point(c(roi[["xmin"]], roi[["ymin"]])),
                      sf::st_point(c(roi[["xmax"]], roi[["ymax"]])),
                      crs = "EPSG:4326")

# Retrieve occurrences for the region of interest
# 99,999; 10,000
res <- rgbif::occ_search(taxonKey = as.numeric(key$usageKey), limit = 99999,
                         geometry = roi_bbox)

data_simp_precomp <- data.frame(id = res$data$key, lat = res$data$decimalLatitude,
                                lon = res$data$decimalLongitude) %>%
  dplyr::filter(!is.na(lat) | !is.na(lon))

nrow(data_simp_precomp)

usethis::use_data(data_simp_precomp, overwrite = TRUE, internal = TRUE)
