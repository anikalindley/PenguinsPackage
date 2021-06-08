# test that the two sided output is correct
test_that("my t.test two sided output is correct", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_equal(my_t.test(x = test_data, alternative = "two.sided", mu = 0.3)$test_stat, -2.028417, tolerance = 0.01)
})
# test that the one sided output using the alternative "greater" is correct
test_that("my t.test one sided output is correct", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_equal(my_t.test(x = test_data, alternative = "greater", mu = 0.4)$test_stat, -2.792139, tolerance = 0.01)
})
# test that the one sided output using the alternative "less" is correct
test_that("my t.test one sided output is correct", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_equal(my_t.test(x = test_data, alternative = "less", mu = 0.4)$test_stat, -2.792139, tolerance = 0.01)
})
# test that the functions throws an error when given alternative is not an accepted input
test_that("my t.test throws error", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_error(my_t.test(x = test_data, alternative = "error", mu = 0.3))
})
# test that the funtion returns a list
test_that("my t.test returns list", {
  set.seed(123)
  test_data <- rnorm(50, 0, 1)
  expect_type(my_t.test(x = test_data, alternative = "less", mu = 0.3), "list")
})
