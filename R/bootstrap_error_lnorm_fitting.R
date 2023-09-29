# Function that performs bootstrap error analysis on a dataframe, adds the errors
# to the prediction column, and fits a log-normal distribution to the new predictions.

bootstrap_error_lnorm_fitting <- function(df, prediction, mean = 0, sd =1, nboot = 100,
                              probs = 0.75, mean_para = TRUE) {
  
  # required packages
  package_list <- c("matrixStats", "fitdistrplus")
  lapply(package_list, require, character.only = TRUE)
  

  
  # Initialize a matrix to store the lnorm parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")
  
  
  # Bootstrap error  and add to predictions
  for (i in 1:nboot) {
    
    # Sample from the residuals with replacement
    # bootstrap_samples <- matrix(sample(df[[error]], replace = replace,
    #                                    size = length(df[[error]])
    # ), ncol = 1)
    
    bootstrap_samples <- rnorm(n = nrow(df), mean = mean, sd = sd)
      
    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- as.vector(df[[prediction]]) + bootstrap_samples
    
    
    # Replace zero values in y_bootstrap with original predictions from df[[prediction]]
    y_bootstrap[y_bootstrap <= 0] <- as.vector(df[[prediction]])[y_bootstrap <= 0]
    
    
    
    # COMPUTE SWE
    pred_swe <- as.vector(df$maxv_SNWD) * y_bootstrap
    
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




  # compute mean or quantile of the distr. parameters
  if (mean_para) {
    result <- colMeans(lnorm_params_matrix, na.rm = TRUE)
  } else {
    quantile_value <- quantile(lnorm_params_matrix[, 2], probs = probs, na.rm = TRUE)

    diffs <- abs(lnorm_params_matrix[, 2] - quantile_value)
    closest_row_position <- which.min(diffs)

    result <- lnorm_params_matrix[closest_row_position, ]
  }
   
  
  return(result)
}
