testthat::context("test-estimate_pcf_fast")

################################################################################

testthat::test_that("estimate_pcf returns spatstat.fv object", {

  pcf_est <- estimate_pcf_fast(pattern = species_b)

  testthat::expect_is(pcf_est, "fv")
})
