
rf_prediction_lnorm_test <- function(data, rf_model, probs) {
  
  # Get the number of trees in the random forest model
  n_trees <- length(rf_model)
  
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = n_trees, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")
  
  
  
  for (i in 1:n_trees) {
    
    # Make predictions using the i-th decision tree
    predictions <- predict(rf_model, data, num.trees  = i)$predictions
    
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
  
  
  quantile_value <- quantile(lnorm_params_matrix[, 2], probs = probs, na.rm = TRUE)
  
  diffs <- abs(lnorm_params_matrix[, 2] - quantile_value)
  closest_row_position <- which.min(diffs)
  
  result <- lnorm_params_matrix[closest_row_position, ]
  
  
  return(result)
  
  
}
