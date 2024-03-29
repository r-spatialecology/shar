# context("test-reconstruct_pattern_multi")

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

multi_recon_fun <- reconstruct_pattern_multi(marked_pattern, n_repetitions = 1, max_steps = 100,
                                             verbose = FALSE, w_statistics = c("Dk" = 1, "K" = 0.5, "Hs" = 0.5, "pcf" = 1),
                                             divisor = "d", kernel = "gaussian")

################################################################################

test_that("Output is a long as n_random for reconstruct_pattern_multi", {

  expect_type(multi_recon, type = "list")

  expect_length(multi_recon, n = 3)

})

test_that("Output includes randomizations and original pattern for reconstruct_pattern_multi", {

  expect_true(all(sapply(multi_recon, function(i) i$reference == random)))

  expect_true(all(sapply(multi_recon, function(i) nrow(i$reconstructed) == N)))

})


test_that("Only one pattern returned for n = 1", {

  expect_length(multi_recon_simple, n = 14)

})

test_that("Energy decresead for for reconstruct_pattern_multi", {

  expect_lt(object = multi_recon_simple$energy_current,
            expected = multi_recon_simple$energy_launch)

})

test_that("Test additional arguments of reconstruct_pattern_multi", {

  expect_equal(object = multi_recon_fun$Parameter_setting$w_statistics,
               expected = c("Dk" = 1, "K" = 0.5, "Hs" = 0.5, "pcf" = 1))

  expect_equal(object = multi_recon_fun$Parameter_setting$divisor,
               expected = "d")

  expect_equal(object = multi_recon_fun$Parameter_setting$kernel_arg,
               expected = "gaussian")

})
