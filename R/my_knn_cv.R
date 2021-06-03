#' k-nearest neighbors cross-validation function
#'
#' This function performs cross-validation on predictions made with k-nearest neighbors.
#'
#' @param train Input data frame.
#' @param cl True class of the training data.
#' @param k_nn Integer indicating that the model will use \code{k_nn} nearest neighbors.
#' @param k_cv Integer indicating that we will use \code{k_cv}-fold cross-validation.
#'
#' @return a list with elements class, a vector of the predicted class for each observation,
#'   and cv_err, a numeric with the cross-validation misclassification error.
#'
#' @examples
#' train <- na.omit(my_penguins)[, c(3:6)]
#' cl <- na.omit(my_penguins)[, 1]
#' my_knn_cv(train, cl, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  # variable used to randomly assign observations to folds 1 through k_cv
  fold <- sample(rep(1:k_cv, length = nrow(train)))

  # initialize list to store the prediction of each fold
  prediction <- list(NA, k_cv)

  # initialize matrix to store the misclassification errors for each fold
  cv_err <- matrix(NA, k_cv, 1)

  # initialize list to store output
  output <- list(NA, NA)

  for (i in 1:k_cv) {
    # training data
    my_train <- train[which(fold != i), ]
    # true class of training data
    cl_train <- cl[which(fold != i)]
    # testing data
    my_test <- train[which(fold == i), ]
    # true class of testing data
    cl_test <- cl[which(fold == i)]

    # use knn() to predict class and store predictions for each iteration in the list
    prediction[[i]] <- class::knn(my_train, my_test, cl_train, k_nn)

    # calculate the misclassification rate for each iteration
    cv_err[i, 1] <- mean(cl_test != prediction[[i]])
  }

  # store output of knn() in vector
  class <- as.vector(class::knn(train, train, cl, k_nn))
  # average misclassification error
  cv_error <- mean(cv_err)
  # add class vector to output
  output[[1]] <- class
  # add average misclassification error to output
  output[[2]] <- cv_error
  # print cross-validation error of each fold in a table
  print(as.table(cv_err))
  # return output
  return(output)
}
