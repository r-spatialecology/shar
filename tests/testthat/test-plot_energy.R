# context("test-plot_energy")

pattern_reconstruction <- reconstruct_pattern(pattern = species_a,
                                              n_random = 3, max_runs = 100,
                                              verbose = FALSE)

pattern_fitted <- fit_point_process(pattern = species_a, n_random = 3,
                                    verbose = FALSE)

################################################################################

test_that("plot_energy returns plot", {

  expect_null(plot_energy(pattern_reconstruction))

})

test_that("plot_energy uses colours", {

  expect_null(plot_energy(pattern_reconstruction,
                                    col = c("blue", "green", "red")))

})

test_that("plot_energy returns error", {

  expect_error(plot_energy(pattern = 1:10),
               regexp = "Class of 'pattern' must be 'rd_pat' or 'rd_mar'.")

  expect_error(plot_energy(pattern_fitted),
               regexp = "There is no 'energy_df' slot. Please use pattern reconstruction for valid input data.")

})
