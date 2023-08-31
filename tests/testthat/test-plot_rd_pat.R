testthat::context("test-plot_rd_pat")

pattern_random <- fit_point_process(species_a, n_random = 3, verbose = FALSE)

pattern_random_ni <- fit_point_process(species_b, n_random = 3, return_input = FALSE,
                                       verbose = FALSE)

pattern_random_marks <- pattern_random$randomized[[1]]

marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)

marks_recon <- reconstruct_pattern_marks(pattern = pattern_random_marks,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 1,
                                         verbose = FALSE)

################################################################################

testthat::test_that("plot returns plot", {

  testthat::expect_null(plot(pattern_random, verbose = FALSE, ask = FALSE))

  testthat::expect_null(plot(pattern_random, what = "pp", verbose = FALSE))
})

testthat::test_that("plot returns error if observed is missing", {

  testthat::expect_error(plot(pattern_random_ni, verbose = FALSE),
                         regexp = "Input must include 'observed' pattern.",
                         fixed = TRUE)
})

testthat::test_that("plot works for reconstructed marks", {

  testthat::expect_null(plot(marks_recon, verbose = FALSE))
})

testthat::test_that("plot returns error if what is wrong", {

  testthat::expect_error(plot(pattern_random, what = "wrong", verbose = FALSE),
                         regexp = "Please select either what = 'sf' or what = 'pp'.",
                         fixed = TRUE)
})
