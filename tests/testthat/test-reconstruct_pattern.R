context("reconstruct_pattern")

pattern_input <- spatstat::runifpoint(n = 50)

n_random <- 3
names_random <- paste0("randomized_", 1:n_random)

test_that("Output is a long as n_random for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = pattern_input,
                                             n_random = n_random,
                                             max_runs = 100)

  expect_length(pattern_recon, n = n_random + 1)

})

test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = pattern_input,
                                             n_random = n_random,
                                             max_runs = 100)

  expect_named(pattern_recon, expected = c(names_random, "observed"))

  expect_equal(pattern_recon[[n_random + 1]], expected = pattern_input)

})

test_that("All optional arguments can be used for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = pattern_input,
                                             n_random = n_random,
                                             max_runs = 100,
                                             fitting = TRUE,
                                             comp_fast = TRUE,
                                             verbose = TRUE)

  expect_type(pattern_recon, type = "list")
})

test_that("Input pattern can not be returned for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = pattern_input,
                                             n_random = n_random,
                                             max_runs = 100,
                                             return_input = FALSE)

  expect_named(pattern_recon, expected = names_random)
})
