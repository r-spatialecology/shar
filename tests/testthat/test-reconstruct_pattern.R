# context("test-reconstruct_pattern")

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

pattern_empty <- spatstat.geom::ppp()

################################################################################

test_that("reconstruct_pattern returns correct class", {

  expect_s3_class(pattern_recon_homo, class = "rd_pat")

  expect_s3_class(pattern_recon_cluster, class = "rd_pat")

  expect_s3_class(pattern_recon_hetero, class = "rd_pat")

})

test_that("Output is a long as n_random for reconstruct_pattern", {

  expect_type(pattern_recon_homo$randomized, type = "list")

  expect_length(pattern_recon_homo$randomized, n = 3)

})

test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  expect_named(pattern_recon_homo$randomized, expected = paste0("randomized_", c(1:3)))

  expect_equal(pattern_recon_homo$observed, expected = spatstat.geom::unmark(species_a))

})

test_that("Reconstructed patterns have same number of points", {

  expect_true(all(vapply(pattern_recon_homo$randomized, FUN.VALUE = logical(1),
                         function(x) x$n == species_a$n)))

})

test_that("Input pattern can not be returned for reconstruct_pattern", {

  expect_true(object = is.na(pattern_recon_ni$observed))

})

test_that("Reconstruction stops if e_threshold is reached", {

  energy <- calculate_energy(pattern_recon_energy, verbose = FALSE)

  expect_true(object = all(energy < 0.1))

  expect_true(all(pattern_recon_energy$stop_criterion == "e_threshold"))

})

test_that("simplify works for reconstruct_pattern", {

  expect_s3_class(pattern_recon_simple, "ppp")

})

test_that("reconstruct_pattern returns errors", {

  expect_error(reconstruct_pattern(pattern = species_a, n_random = -5, verbose = FALSE),
               regexp = "n_random must be >= 1.")

  expect_error(reconstruct_pattern(pattern = pattern_empty, n_random = 199),
               regexp = "The observed pattern contains no points.")

})

test_that("reconstruct_pattern returns warnings", {

  expect_warning(reconstruct_pattern(pattern = species_a, n_random = 2, max_runs = 1,
                                     return_input = FALSE, simplify = TRUE,
                                     verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  expect_warning(reconstruct_pattern(pattern = species_a, n_random = 1, max_runs = 1,
                                     simplify = TRUE, verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")

})
