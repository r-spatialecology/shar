testthat::context("test-extract_points")

testthat::test_that("extract_points returns one row for each habitat", {

  landscape_classified <- SHAR::classify_habitats(SHAR::landscape, classes = 3)

  points_df <- SHAR::extract_points(raster = landscape_classified,
                                    pattern = SHAR::species_a)

  testthat::expect_equal(nrow(points_df), expected = 3)
})

testthat::test_that("extract_points counts all points present in the landscape", {

  landscape_classified <- SHAR::classify_habitats(SHAR::landscape, classes = 3)

  points_df <- SHAR::extract_points(raster = landscape_classified,
                                    pattern = SHAR::species_b)

  extracted_points <- sum(points_df$count)

  testthat::expect_equal(extracted_points, expected = SHAR::species_b$n)
})
