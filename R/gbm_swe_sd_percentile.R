
gbm_swe_sd_percentile <- function(data, prediction, mean = 0, sd =1, nboot = 200) {
  
  # required packages
  package_list <- c("matrixStats", "fitdistrplus")
  lapply(package_list, require, character.only = TRUE)
  
  
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")
  
  
  
  for (i in 1:nboot) {
    
    bootstrap_samples <- rnorm(n = nrow(data), mean = mean, sd = sd)
    
    # Add the bootstrapped residuals to the full data predictions
  #  y_bootstrap <- as.vector(data[[prediction]]) + bootstrap_samples
    
    
    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- ifelse((as.vector(data[[prediction]]) + bootstrap_samples) <= 0,
                          as.vector(data[[prediction]]),
                          (as.vector(data[[prediction]]) + bootstrap_samples))
    
    
    
    # COMPUTE SWE
    pred_swe <- as.vector(data$maxv_SNWD) * y_bootstrap
    
    # Fit a GEV distribution to the predictions
    lnorm_model <- fitdist(
      data = as.numeric(pred_swe), distr = "lnorm",
      start = list(
        meanlog = mean(log(pred_swe)), sdlog = stats::sd(log(pred_swe))
      ),
      method = "mle"
    )
    
    
    # Store the parameters in the matrix
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
  percentile = sum(lnorm_params_matrix[, "sdlog"] <= closest_sd) / nboot
  
  
  return(percentile)
  
  
}
