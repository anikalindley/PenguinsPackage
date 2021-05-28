my_lm <- function(formula, data) {

  # extract the model matrix
  matrix_x <- model.matrix(formula, data)

  # extract the model frame object
  model_frame <- model.frame(formula, data)

  # extract the model reponse
  response_y <- model.response(model_frame)

  # solve for linear regression coefficents
  beta_hat <- (solve(t(matrix_x) %*% matrix_x) %*% t(matrix_x) %*% response_y)

  # degrees of freedom; sample size minus number of covariates
  my_df2 <- (nrow(matrix_x) - ncol(matrix_x))

  # calculate the variance
  variance <- sum(((response_y - matrix_x %*% beta_hat)^2) / (my_df2))

  # calculate the standard error
  std_err <- sqrt(diag(variance * solve(t(matrix_x) %*% matrix_x)))

  # calculate the t-value
  t_val <- ((beta_hat - 0) / std_err)

  # calculate the probability of getting a test statistic at least as extreme as the one I observed, assuming the null hypothesis is correct
  prob <- 2 * pt(abs(t_val), df = my_df2, lower.tail = FALSE)

  # add coefficient estimate, standard error, t value, and probability to the table
  my_table <- data.frame("Estimate" = beta_hat,
                         "Std. Error" = std_err,
                         "t value" = t_val,
                         "Pr(>|t|)" = prob)

  # convert data frame to table
  my_table <- as.table(as.matrix(my_table))

  # rename columns
  colnames(my_table)[2] <- "Std. Error"
  colnames(my_table)[3] <- "t value"
  colnames(my_table)[4] <- "Pr(>|t|)"

  # return the table
  return(my_table)

}
