context("Fit point process")

pattern_input <- spatstat::runifpoint(n = 50)

n_random <- 3
names_random <- paste0("randomized_", 1:n_random)

test_that("Output is a long as n_random for fit_point_process", {

  pattern_recon <- SHAR::fit_point_process(pattern = pattern_input,
                                           n_random = n_random)

  expect_length(pattern_recon, n = n_random + 1)
})

test_that("Output includes randomizations and original pattern for fit_point_process", {

  pattern_recon <- SHAR::fit_point_process(pattern = pattern_input,
                                           n_random = n_random)

  expect_named(pattern_recon, expected = c(names_random, "observed"))

  expect_equal(pattern_recon[[n_random + 1]], expected = pattern_input)
})

test_that("All optional arguments can be used for fit_point_process", {

  pattern_recon <- SHAR::fit_point_process(pattern = pattern_input,
                                           n_random = n_random,
                                           process = "cluster")

  expect_type(pattern_recon, type = "list")
})

test_that("Input pattern can not be returned for fit_point_process", {

  pattern_recon <- SHAR::fit_point_process(pattern = pattern_input,
                                           n_random = n_random,
                                           return_input = FALSE)

  expect_named(pattern_recon, expected = names_random)
})
