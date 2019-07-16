testthat::context("test-print.rd_ras")

landscape_classified <- shar::classify_habitats(landscape, classes = 5)

landscape_random <- shar::randomize_raster(landscape_classified, n_random = 1)

################################################################################

testthat::test_that("print.rd_pat works", {

  output <- testthat::capture_output(shar::print.rd_ras(landscape_random))

  testthat::expect_equal(object = output, expected = "No. of raster: 2\nMethod: randomize_raster()\nObserved pattern: included")
})
