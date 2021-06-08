#' Random forest cross-validation
#"
#' This function uses cross validation to assess how predictions made using
#' random forest models can generalize to independent data sets.
#'
#' @param k Numeric indicating that the function will use \code{k}-fold cross-validation.
#'
#' @keywords prediction
#'
#' @return Numeric with the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # load penguin data
  penguins2 <- project3package::my_penguins
  penguins2 <- stats::na.omit(penguins2)[, c(3:6)]

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
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                          data = penguin_train,
                          ntree = 100)

    # prediction made using testing data
    weight_prediction <- stats::predict(model, penguin_test[, -4])

    # calculate and store the average mean squared error for each fold
    MSE[i, 1] <- mean((weight_prediction - penguin_test$body_mass_g)^2)
  }

  # average mean squared error across iterations
  CV_MSE <- mean(MSE)
  # return average mean squared error
  return(CV_MSE)
}

