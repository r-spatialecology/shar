testthat::context("test-extract_points")

landscape_classified <- shar::classify_habitats(shar::landscape, classes = 5)

points_df <- shar::extract_points(raster = landscape_classified,
                                  pattern = shar::species_b)

################################################################################

testthat::test_that("extract_points returns one row for each habitat", {

  testthat::expect_equal(nrow(points_df),
                         expected = 5)
})

testthat::test_that("extract_points counts all points present in the landscape", {

  extracted_points <- sum(points_df$count)

  testthat::expect_equal(extracted_points, expected = shar::species_b$n)
})
