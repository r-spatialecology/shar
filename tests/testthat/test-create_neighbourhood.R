context("test-create_neighbourhood")

test_that("create_neighbourhood returns right dimension for directions = 4", {

  mat <- matrix(1, nrow= 10, ncol = 10)

  cell_id <- rbind(cbind(3,5), cbind(7,1))

  neighbourhood <- create_neighbourhood(cell_id, mat, directions = 4)

  expect_equal(nrow(neighbourhood), expected = 7) # only 7 because one cell_id is at boundary
  expect_equal(ncol(neighbourhood), expected = 2)
})

test_that("create_neighbourhood returns right dimension for directions = 8", {

  mat <- matrix(1, nrow= 10, ncol = 10)

  cell_id <- rbind(cbind(3,5), cbind(7,1))

  neighbourhood <- create_neighbourhood(cell_id, mat, directions = 8)

  expect_equal(nrow(neighbourhood), expected = 13) # only 7 because one cell_id is at boundary
  expect_equal(ncol(neighbourhood), expected = 2)
})
