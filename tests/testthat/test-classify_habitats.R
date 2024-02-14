# context("test-classify_habitats")

landscape_classified <- classify_habitats(raster = terra::rast(landscape), n = 5, style = "fisher")

landscape_classified_brks <- classify_habitats(raster = terra::rast(landscape),
                                               fixedBreaks = c(0, 0.25, 0.75, 1.0),
                                               style = "fixed", return_breaks = TRUE)

################################################################################

test_that("classify_habitats returns n classes", {

  present_classes <- length(unique(terra::values(landscape_classified)))

  expect_equal(present_classes, expected = 5)

})

test_that("classify_habitats useses breaks and returns them", {

  expect_type(object = landscape_classified_brks, type = "list")

  expect_equal(object = landscape_classified_brks$breaks$brks,
               expected = c(0, 0.25, 0.75, 1.0))

  present_classes <- length(unique(terra::values(landscape_classified_brks$raster)))

  expect_equal(present_classes, expected = 3)

})
