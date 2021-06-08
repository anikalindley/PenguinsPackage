# test that the output is a list
test_that("my_knn_cv output is correct ", {
  penguins_test <- project3package::my_penguins
  penguins_test <- stats::na.omit(penguins_test)
  expect_type(my_knn_cv(penguins_test[, c(3:6)], penguins_test[, 1], 1, 5), "list")
})
