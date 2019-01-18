testthat::context("test-extract_points")

testthat::test_that("extract_points returns one row for each habitat", {

  landscape_classified <- shar::classify_habitats(shar::landscape, classes = 3)

  points_df <- shar::extract_points(raster = landscape_classified,
                                    pattern = shar::species_a)

  testthat::expect_equal(nrow(points_df), expected = 3)
})

testthat::test_that("extract_points counts all points present in the landscape", {

  landscape_classified <- shar::classify_habitats(shar::landscape, classes = 3)

  points_df <- shar::extract_points(raster = landscape_classified,
                                    pattern = shar::species_b)

  extracted_points <- sum(points_df$count)

  testthat::expect_equal(extracted_points, expected = shar::species_b$n)
})
