testthat::context("test-reconstruct_pattern")

# normal reconstruction
pattern_recon_homo <- reconstruct_pattern(pattern = species_a, n_random = 3,
                                          max_runs = 1, verbose = FALSE)

# cluster reconstruction
pattern_recon_cluster <- reconstruct_pattern(pattern = species_a, n_random = 3, method = "cluster",
                                             max_runs = 1, verbose = FALSE)

# cluster reconstruction
pattern_recon_hetero <- reconstruct_pattern(pattern = species_b, n_random = 3, method = "hetero",
                                            max_runs = 1, verbose = FALSE)

pattern_recon_ni <- reconstruct_pattern(pattern = species_a, n_random = 3,
                                        max_runs = 1, return_input = FALSE,
                                        verbose = FALSE)

pattern_recon_energy <- reconstruct_pattern(pattern = species_a, max_runs = 1000,
                                            e_threshold = 0.1, n_random = 3,
                                            verbose = FALSE)

pattern_recon_simple <- reconstruct_pattern(pattern = species_a, n_random = 1,
                                            max_runs = 1, simplify = TRUE, return_input = FALSE,
                                            verbose = FALSE)

################################################################################

testthat::test_that("reconstruct_pattern returns correct class", {

  testthat::expect_is(pattern_recon_homo, class = "rd_pat")

  testthat::expect_is(pattern_recon_cluster, class = "rd_pat")

  testthat::expect_is(pattern_recon_hetero, class = "rd_pat")

})

testthat::test_that("Output is a long as n_random for reconstruct_pattern", {

  testthat::expect_type(pattern_recon_homo$randomized, type = "list")

  testthat::expect_length(pattern_recon_homo$randomized, n = 3)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  testthat::expect_named(pattern_recon_homo$randomized,
                         expected = paste0("randomized_", c(1:3)))

  testthat::expect_equal(pattern_recon_homo$observed,
                         expected = spatstat.geom::unmark(species_a))
})

testthat::test_that("Reconstructed patterns have same number of points", {

  testthat::expect_true(all(vapply(pattern_recon_homo$randomized,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == species_a$n)))
})

testthat::test_that("Input pattern can not be returned for reconstruct_pattern", {

  testthat::expect_equal(object = pattern_recon_ni$observed,
                         expected = "NA")
})

testthat::test_that("Reconstruction stops if e_threshold is reached", {

  energy <- calculate_energy(pattern_recon_energy, verbose = FALSE)

  testthat::expect_true(object = all(energy < 0.1))

  testthat::expect_true(all(pattern_recon_energy$stop_criterion == "e_threshold"))

})

testthat::test_that("simplify works for reconstruct_pattern", {

  testthat::expect_is(pattern_recon_simple, "ppp")
})

testthat::test_that("reconstruct_pattern returns error if n_random < 1", {

  testthat::expect_error(reconstruct_pattern(pattern = species_a, n_random = -5,
                                             verbose = FALSE),
                         regexp = "n_random must be >= 1.")
})

testthat::test_that("reconstruct_pattern returns warnings", {

  testthat::expect_warning(reconstruct_pattern(pattern = species_a,
                                               n_random = 2, max_runs = 1,
                                               return_input = FALSE, simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  testthat::expect_warning(reconstruct_pattern(pattern = species_a,
                                               n_random = 1, max_runs = 1,
                                               simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")
})
