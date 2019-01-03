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

  expect_equal(pattern_random[[20]], expected = spatstat::unmark(SHAR::species_a))
})

test_that("Fitted patterns have same number of points for cluster process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_b,
                                            n_random = 199,
                                            process = "cluster",
                                            return_input = FALSE)

  expect_true( all(purrr::map_lgl(pattern_random, function(x) x$n == SHAR::species_b$n)))
})

test_that("Input pattern can not be returned for fit_point_process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_b,
                                            n_random = 19,
                                            return_input = FALSE)

  expect_false(any(SHAR::species_b %in% pattern_random))
})

test_that("simplify works for fit_point_process", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 1,
                                            return_input = FALSE,
                                            simplify = TRUE)

  expect_is(pattern_random, "ppp")
})

test_that("fit_point_process returns errors", {

  expect_error(SHAR::fit_point_process(pattern = SHAR::species_b,
                                       n_random = -10),
               regexp = "n_random must be >= 1.")

  expect_error(SHAR::fit_point_process(pattern = SHAR::species_b,
                                       n_random = 19,
                                       process = "not_valid"),
               regexp = "Please select either 'poisson' or 'cluster'.")
})

test_that("fit_point_process returns warnings", {

  expect_warning(SHAR::fit_point_process(pattern = SHAR::species_b,
                                         n_random = 19,
                                         return_input = FALSE,
                                         simplify = TRUE,
                                         verbose = TRUE),
               regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  expect_warning(SHAR::fit_point_process(pattern = SHAR::species_b,
                                         n_random = 1,
                                         simplify = TRUE,
                                         verbose = TRUE),
                 regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")
})

