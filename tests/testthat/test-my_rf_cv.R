test_that("my_rf_cv error ", {
  expect_error(my_rf_cv("string"))
})

test_that("my_rf_cv output is correct ", {
  expect_true(is.numeric(my_rf_cv(5)))
})

test_that("my_rf_cv outputs a numeric", {
  expect_type(my_rf_cv(5), "double")
})
