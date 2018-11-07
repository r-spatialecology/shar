context("test-classify_habitats")

test_that("classify_habitats returns n classes", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  present_classes <- length(unique(raster::values(landscape_classified)))

  expect_equal(present_classes, expected = 3)
})
