testthat::context("test-classify_habitats")

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

################################################################################

testthat::test_that("classify_habitats returns n classes", {

  present_classes <- length(unique(raster::values(landscape_classified)))

  testthat::expect_equal(present_classes, expected = 5)
})
