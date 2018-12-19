context("test-plot_randomized_pattern")

test_that("plot_randomized_pattern returns plot", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_b, n_random = 39)

  plot_result <- plot_randomized_pattern(pattern_random)

  expect_is(plot_result, class = "list")
})


test_that("plot_randomized_pattern returns error if input is named wrong", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_b,
                                            n_random = 39,
                                            return_input = FALSE)

  expect_error(plot_randomized_pattern(pattern_random),
               regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})

test_that("plot_randomized_pattern uses comp_fast", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_b,
                                            n_random = 39)

  plot_result <- plot_randomized_pattern(pattern_random, comp_fast = TRUE)

  expect_is(plot_result, class = "list")
})

