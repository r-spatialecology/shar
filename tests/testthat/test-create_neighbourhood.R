testthat::context("test-create_neighbourhood")

mat <- matrix(1, nrow= 10, ncol = 10)

cell_id <- rbind(cbind(3,5), cbind(7,1))

neighbourhood_4 <- shar::create_neighbourhood(cell_id, mat, directions = 4)

neighbourhood_8 <- shar::create_neighbourhood(cell_id, mat, directions = 8)


testthat::test_that("create_neighbourhood returns right dimension for directions = 4", {

  testthat::expect_equal(nrow(neighbourhood_4), expected = 7) # only 7 because one cell_id is at boundary

  testthat::expect_equal(ncol(neighbourhood_4), expected = 2)
})

testthat::test_that("create_neighbourhood returns right dimension for directions = 8", {

  testthat::expect_equal(nrow(neighbourhood_8), expected = 13) # only 7 because one cell_id is at boundary

  testthat::expect_equal(ncol(neighbourhood_8), expected = 2)
})
