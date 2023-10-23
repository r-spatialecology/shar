# testthat::context("test-plot_rd_pat")

pattern_random <- fit_point_process(species_a, n_random = 9, verbose = FALSE)

################################################################################

test_that("sample_randomized returns n = 3 for no user input", {

  expect_length(object = sample_randomized(randomized = pattern_random$randomized,
                                           verbose = FALSE), n = 3)
})

test_that("sample_randomized returns selected ids", {

  expect_named(object = sample_randomized(randomized = pattern_random$randomized,
                                           n = c(3, 6, 9), verbose = FALSE),
               expected = paste0("randomized_", c(3, 6, 9)))
})

test_that("sample_randomized returns warnings", {

  expect_warning(object = sample_randomized(randomized = pattern_random$randomized,
                                          n = 199, verbose = TRUE),
                 regexp = "n larger than number of randomize eleements. Setting n = 3")
})

test_that("sample_randomized returns warnings", {

  expect_warning(object = sample_randomized(randomized = pattern_random$randomized,
                                            n = 199, verbose = TRUE),
                 regexp = "n larger than number of randomize eleements. Setting n = 3.")

  expect_warning(object = sample_randomized(randomized = pattern_random$randomized,
                                            n = c(1, 199), verbose = TRUE),
                 regexp = "Using only IDs that are present in randomized data.")
})

test_that("sample_randomized returns error", {

  expect_error(object = sample_randomized(randomized = pattern_random$randomized,
                                          n = c(150, 199), verbose = TRUE),
               regexp = "Please provide at least on valid ID for n.")
})
