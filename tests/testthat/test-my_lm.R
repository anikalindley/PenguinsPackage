test_that("my_lm intercept is correct", {
  expect_equal(my_lm(formula = mtcars$wt ~ mtcars$cyl + mtcars$mpg, data = mtcars)[1,1], 5.07, tolerance = .01)
})

test_that("my_lm output is correct", {
  test1 = my_lm(formula = mtcars$wt ~ mtcars$cyl + mtcars$mpg, data = mtcars)
  expect_true(is.table(test1))
})
