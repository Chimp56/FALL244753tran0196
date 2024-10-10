# Test for the mean component
testthat::test_that("mu is correctly returned", {
  result <- myncurve(0, 1, 1.5)
  testthat::expect_equal(result$mu, 0)
})

# Test for the sigma component
testthat::test_that("sigma is correctly returned", {
  result <- myncurve(0, 1, 1.5)
  testthat::expect_equal(result$sigma, 1)
})

# Test for the calculated area (probability)
testthat::test_that("area (probability) is correctly calculated", {
  result <- myncurve(0, 1, 1.5)
  expected_prob <- pnorm(1.5, mean = 0, sd = 1)
  testthat::expect_equal(result$area, expected_prob)
})
