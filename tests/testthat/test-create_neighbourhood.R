# context("test-create_neighbourhood")

mat <- matrix(1, nrow = 10, ncol = 10)

cell_id <- rbind(cbind(3,5), cbind(7,1))

neighbourhood_4 <- create_neighbourhood(cell_id, mat, directions = 4)

neighbourhood_8 <- create_neighbourhood(cell_id, mat, directions = 8)

################################################################################

test_that("create_neighbourhood returns right dimension for directions = 4", {

  expect_equal(nrow(neighbourhood_4), expected = 7) # only 7 because one cell_id is at boundary

  expect_equal(ncol(neighbourhood_4), expected = 2)

})

test_that("create_neighbourhood returns right dimension for directions = 8", {

  expect_equal(nrow(neighbourhood_8), expected = 13) # only 7 because one cell_id is at boundary

  expect_equal(ncol(neighbourhood_8), expected = 2)

})

test_that("create_neighbourhood returns error", {

  expect_error(create_neighbourhood(cell_id, mat, directions = 12),
               regexp = "'directions must be 'directions = 4' or 'directions = 8'.")

})
