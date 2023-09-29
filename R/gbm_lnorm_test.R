
gbm_lnorm_test <- function(data, gbm_model, mean = 0, sd = 1, probs, nboot = 200) {
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")



  for (i in 1:nboot) {
    # Make predictions using the i-th decision tree
    predictions <- gbm::predict.gbm(gbm_model, n.trees = gbm_model$n.trees, data)

    bootstrap_samples <- rnorm(n = nrow(data), mean = mean, sd = sd)

    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- ifelse((predictions + bootstrap_samples) <= 0,
      predictions, (predictions + bootstrap_samples)
    )


    # compute SWE
    pred_swe <- as.vector(data$maxv_SNWD) * y_bootstrap

    # Fit a distribution to the predictions
    lnorm_model <- fitdist(
      data = as.numeric(pred_swe), distr = "lnorm",
      start = list(
        meanlog = mean(log(as.numeric(pred_swe))),
        sdlog = stats::sd(log(as.numeric(pred_swe)))
      ),
      method = "mle"
    )


    # Store the  parameters in the matrix
    lnorm_params_matrix[i, ] <- lnorm_model$estimate
  }


  quantile_value <- quantile(lnorm_params_matrix[, 2], probs = probs, na.rm = TRUE)

  diffs <- abs(lnorm_params_matrix[, 2] - quantile_value)
  closest_row_position <- which.min(diffs)

  result <- lnorm_params_matrix[closest_row_position, ]


  return(result)
}
