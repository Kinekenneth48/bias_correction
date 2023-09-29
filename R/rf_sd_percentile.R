
rf_sd_percentile <- function(data, rf_model) {
  
  # Get the number of trees in the random forest model
  n_trees <- length(rf_model)
  
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = n_trees, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")
  
  
  
  for (i in 1:n_trees) {
    
    # Make predictions using the i-th decision tree
    predictions <- predict(rf_model[[i]], data)$predictions
    
    # compute SWE
    pred_swe <- data$maxv_SNWD * predictions
    
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
  
  # Fit a lognormal distribution to the true SWE
  fit_true <- fitdistrplus::fitdist(
    data = data[["maxv_WESD"]], distr = "lnorm",
    start = list(
      meanlog = mean(log(data[["maxv_WESD"]])), 
      sdlog = stats::sd(log(data[["maxv_WESD"]]))
    ),
    method = "mle"
  )
  
  # Get the estimated standard deviation of the true data
  sd_true <- fit_true$estimate[["sdlog"]]
  
  # Find the percentile of the sdlog that is closest to sd_true
  closest_sd_index <- which.min(abs(lnorm_params_matrix[, "sdlog"] - sd_true))
  closest_sd <- lnorm_params_matrix[closest_sd_index, "sdlog"]
  percentile = sum(lnorm_params_matrix[, "sdlog"] <= closest_sd) / n_trees
  
  
  return(percentile)
  
  
}
