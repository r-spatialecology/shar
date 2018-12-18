context("test-plot_randomized_pattern")

test_that("plot_randomzed_pattern returns ggplot", {

  pattern_recon <- SHAR::reconstruct_pattern(SHAR::species_a, n_random = 3, max_runs = 10)

  plot_result <- plot_randomized_pattern(pattern_recon)

  expect_is(plot_result, class = "list")
})
