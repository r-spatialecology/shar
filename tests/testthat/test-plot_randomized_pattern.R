context("test-plot_randomized_pattern")

test_that("plot_randomzed_pattern returns ggplot", {
  pattern_recon <- SHAR::reconstruct_pattern(SHAR::species_a, n_random = 9, max_runs = 1000)
  ggplot_result <- plot_randomized_pattern(pattern_recon)

  expect_is(ggplot_result, class = "ggplot")
})
