#' random forest cross-validation
#"
#' This function performs cross validation on predictions made using random forest
#'
#' @param k Numeric indicating the number of folds used for cross-validation
#'
#' @return Numeric with the cross-validation error
my_rf_cv <- function(k) {
  # variable used to randomly assign observations to folds 1 through k
  fold <- sample(rep(1:k, length = nrow(penguins2)))

  # initialize variable to store the misclassification errors
  MSE <- matrix(NA, k, 1)

  for (i in 1:k) {
    # training data
    penguin_train <- penguins2[which(fold != i), ]
    # testing data
    penguin_test <- penguins2[which(fold == i), ]

    # use random forest to train model using 100 trees
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                          data = penguin_train,
                          ntree = 100)

    # prediction made using testing data
    weight_prediction <- predict(model, penguin_test[, -1])

    # calculate and store the average mean squared error for each fold
    MSE[i, 1] <- mean(weight_prediction - penguin_test$body_mass_g)^2
  }

  # average mean squared error across iterations
  CV_MSE <- mean(MSE)
  # return average mean squared error
  return(CV_MSE)
}

# select covariates and column containing true body mass observations
penguins2 <- palmerpenguins::penguins %>% select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm)
# remove NA values
penguins2 <- penguins2 %>% na.omit()

# run function with 5-fold cross validation
my_rf_cv(5)
