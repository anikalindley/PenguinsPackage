test_that("my t.test output is correct", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_equal(my_t.test(x = test_data, alternative = "two.sided", mu = 0.3)$test_stat, -2.028417, tolerance = 0.01)
})

test_that("my t.test throws error", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_error(my_t.test(x = test_data, alternative = "error", mu = 0.3))
})
