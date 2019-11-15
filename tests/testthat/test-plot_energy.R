testthat::context("test-plot_energy")

pattern_reconstruction <- shar::reconstruct_pattern_homo(pattern = shar::species_a,
                                                         n_random = 3,
                                                         max_runs = 100,
                                                         verbose = FALSE)

pattern_fitted <- shar::fit_point_process(pattern = shar::species_a,
                                          n_random = 3,
                                          verbose = FALSE)

################################################################################

testthat::test_that("plot_energy returns plot", {

  testthat::expect_null(shar::plot_energy(pattern_reconstruction))
})

testthat::test_that("plot_energy uses colours", {

  testthat::expect_null(shar::plot_energy(pattern_reconstruction,
                                          col = c("blue", "green", "red")))
})

testthat::test_that("plot_energy returns error", {

  testthat::expect_error(shar::plot_energy(pattern = 1:10),
                         regexp = "Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
                         fixed = TRUE)

  testthat::expect_error(shar::plot_energy(pattern_fitted),
                         regexp = "There is no 'energy_df' slot. Please use pattern reconstruction for valid input data.",
                         fixed = TRUE)
})
