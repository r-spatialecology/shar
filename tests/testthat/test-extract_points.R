# context("test-extract_points")

landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")

points_df <- extract_points(raster = landscape_classified, pattern = species_b)

################################################################################

test_that("extract_points returns one row for each habitat", {

  expect_equal(nrow(points_df), expected = 5)

})

test_that("extract_points counts all points present in the landscape", {

  extracted_points <- sum(points_df$count)

  expect_equal(extracted_points, expected = species_b$n)

})
