# testthat::context("test-reconstruct_pattern_multi")

# create random data
xr <- 500
yr <- 1000

N <- 400

y <- runif(N, min = 0, max = yr)
x <- runif(N, min = 0, max = xr)

species <- sample(c("A","B"), N, replace = TRUE)
diameter <- runif(N, 0.1, 0.4)

random <- data.frame(x = x, y = y, dbh = diameter, species = factor(species))

# Conversion to a ppp object and conversion of the metric mark to metres.
marked_pattern <- spatstat.geom::as.ppp(random, W = spatstat.geom::owin(c(0, 500), c(0, 1000)))

# Reconstruction function
multi_recon <- reconstruct_pattern_multi(marked_pattern, n_repetitions = 3, max_steps = 10,
                                            verbose = FALSE)

multi_recon_simple <- reconstruct_pattern_multi(marked_pattern, n_repetitions = 1, max_steps = 100,
                                                verbose = FALSE)

################################################################################

testthat::test_that("Output is a long as n_random for reconstruct_pattern_multi", {

  testthat::expect_type(multi_recon, type = "list")

  testthat::expect_length(multi_recon, n = 3)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_pattern_multi", {

  testthat::expect_true(all(sapply(multi_recon, function(i) i$reference == random)))

  testthat::expect_true(all(sapply(multi_recon, function(i) nrow(i$reconstructed) == N)))
})


testthat::test_that("Only one pattern returned for n = 1", {

  testthat::expect_length(multi_recon_simple, n = 14)
})

testthat::test_that("Energy decresead for for reconstruct_pattern_multi", {

  testthat::expect_lt(object = multi_recon_simple$energy_current,
                      expected = multi_recon_simple$energy_launch)
})
