testthat::context("test-reconstruct_pattern")

# normal reconstruction
pattern_recon_homo <- reconstruct_pattern(pattern = species_a, n_random = 3, method = "homo",
                                          max_runs = 1, verbose = FALSE)

# cluster reconstruction
pattern_recon_cluster <- reconstruct_pattern(pattern = species_a, n_random = 3, method = "cluster",
                                             max_runs = 1, verbose = FALSE)

# cluster reconstruction
pattern_recon_hetero <- reconstruct_pattern(pattern = species_b, n_random = 3, method = "hetero",
                                             max_runs = 1, verbose = FALSE)

################################################################################

testthat::test_that("reconstruct_pattern returns correct class", {

  testthat::expect_is(pattern_recon_homo, class = "rd_pat")

  testthat::expect_is(pattern_recon_cluster, class = "rd_pat")

  testthat::expect_is(pattern_recon_hetero, class = "rd_pat")

})

testthat::test_that("reconstruct_patternreturns warnings", {

  testthat::expect_warning(reconstruct_pattern(pattern = species_a, n_random = 3,
                                               method = "cluster", n_points = 5,
                                               max_runs = 1),

                           regexp = "'n_points', 'window', or 'r_max' are not used for method='cluster'.")

  testthat::expect_warning(reconstruct_pattern(pattern = species_a, n_random = 3,
                                               method = "hetero", n_points = 5,
                                               max_runs = 1),

                           regexp = "'n_points', 'window', or 'r_max' are not used for method='hetero'.")
})
