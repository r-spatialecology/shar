testthat::context("test-classify_habitats")

landscape_classified <- classify_habitats(raster = landscape, classes = 5)

landscape_classified_brks <- classify_habitats(raster = landscape, classes = c(0, 0.25, 0.75, 1.0),
                                               return_classes = TRUE)

################################################################################

testthat::test_that("classify_habitats returns n classes", {

  present_classes <- length(unique(raster::values(landscape_classified)))

  testthat::expect_equal(present_classes, expected = 5)
})

testthat::test_that("classify_habitats useses breaks and returns them", {

  testthat::expect_type(object = landscape_classified_brks, type = "list")

  testthat::expect_equal(object = landscape_classified_brks$classes,
                         expected = c(0, 0.25, 0.75, 1.0))

  present_classes <- length(unique(raster::values(landscape_classified_brks$raster)))

  testthat::expect_equal(present_classes, expected = 3)
})
