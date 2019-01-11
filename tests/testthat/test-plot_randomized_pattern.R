testthat::context("test-plot_randomized_pattern")

testthat::test_that("plot_randomized_pattern returns plot", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_b, n_random = 19)

  plot_result <- SHAR::plot_randomized_pattern(pattern_random)

  testthat::expect_is(plot_result, class = "list")
})

testthat::test_that("plot_randomized_pattern returns error if input is named wrong", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_b,
                                            n_random = 19,
                                            return_input = FALSE)

  testthat::expect_error(SHAR::plot_randomized_pattern(pattern_random),
                         regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})

testthat::test_that("plot_randomized_pattern uses comp_fast", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_b,
                                            n_random = 19)

  SHAR::plot_randomized_pattern(pattern_random, comp_fast = TRUE)
})

testthat::test_that("plot_randomized_patterns works for reconstructed marks", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_a, n_random = 19)[[1]]

  marks_sub <- spatstat::subset.ppp(species_a, select = dbh)

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_random,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 10)

  SHAR::plot_randomized_pattern(marks_recon, method = "marks")
})

testthat::test_that("plot_randomized_patterns returns error if method is wrong", {

  pattern_random <- SHAR::fit_point_process(SHAR::species_a, n_random = 19)[[1]]

  marks_sub <- spatstat::subset.ppp(SHAR::species_a, select = dbh)

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_random,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 10)

  testthat::expect_error(SHAR::plot_randomized_pattern(marks_recon,
                                                       method = "wrong"),
                         regexp = "'method' must be either 'method = 'spatial'' or 'method = 'marks''")
  })
