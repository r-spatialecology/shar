context("Fit point process")

test_that("Output is a long as n_random for fit_point_process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  expect_length(pattern_random, n = 20)
})

test_that("Output includes randomizations and original pattern for fit_point_process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  expect_named(pattern_random, expected = c(paste0("randomized_", 1:19), "observed"))

  expect_equal(pattern_random[[20]], expected = SHAR::species_a)
})

test_that("All optional arguments can be used for fit_point_process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_b,
                                            n_random = 19,
                                            process = "cluster")

  expect_type(pattern_random, type = "list")
})

test_that("Input pattern can not be returned for fit_point_process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_b,
                                           n_random = 19,
                                           return_input = FALSE)

  expect_false(any(SHAR::species_b %in% pattern_random))
})
