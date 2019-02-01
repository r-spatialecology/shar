testthat::context("test-plot_randomized_pattern")

pattern_random_a <- shar::fit_point_process(shar::species_a, n_random = 3)

pattern_random_b <- shar::fit_point_process(shar::species_b,
                                          n_random = 3,
                                          return_input = FALSE)

pattern_random_marks <- pattern_random_a[[1]]

marks_sub <- spatstat::subset.ppp(species_a, select = dbh)

marks_recon <- shar::reconstruct_marks(pattern = pattern_random_marks,
                                       marked_pattern = marks_sub,
                                       n_random = 3, max_runs = 1)

testthat::test_that("plot_randomized_pattern returns plot", {

  plot_result <- shar::plot_randomized_pattern(pattern_random_a)

  testthat::expect_is(plot_result, class = "list")
})

testthat::test_that("plot_randomized_pattern returns error if input is named wrong", {

  testthat::expect_error(shar::plot_randomized_pattern(pattern_random_b),
                         regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})

testthat::test_that("plot_randomized_pattern uses comp_fast", {

  shar::plot_randomized_pattern(pattern_random_a,
                                comp_fast = TRUE)
})

testthat::test_that("plot_randomized_patterns works for reconstructed marks", {

  shar::plot_randomized_pattern(marks_recon,
                                method = "marks")
})

testthat::test_that("plot_randomized_patterns returns error if method is wrong", {

  testthat::expect_error(shar::plot_randomized_pattern(marks_recon,
                                                       method = "wrong"),
                         regexp = "'method' must be either 'method = 'spatial'' or 'method = 'marks''")
  })
