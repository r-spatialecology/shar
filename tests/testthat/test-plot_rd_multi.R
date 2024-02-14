# context("test-plot_rd_multi")

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

multi_recon_simple <- reconstruct_pattern_multi(marked_pattern, n_repetitions = 1, max_steps = 10,
                                         verbose = FALSE)

################################################################################

test_that("plot returns plot", {

  expect_null(plot(multi_recon, verbose = FALSE))
  expect_null(plot(multi_recon_simple, verbose = FALSE))

})
