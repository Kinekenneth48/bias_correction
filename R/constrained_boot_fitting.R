constrained_boot_fitting <- function(data, keep_indices, n_trees, 
                                     constrained_boot = TRUE) {
  
  # Initialize a matrix to store the distribution parameters
  lnorm_params_matrix <- matrix(nrow = n_trees, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")


  if (constrained_boot) {
    
    # Fit different decision trees and make predictions
    for (i in 1:n_trees) {
      
      synthetic_data <- rbind(
        data[c(sample(setdiff(1:nrow(data), keep_indices), replace = TRUE)), ],
        data[c(keep_indices), ]
      )

      # Fit a single decision tree
      tree_model <- ranger(
        RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
          TD + ELEV,
        data = synthetic_data, importance = "impurity", num.trees = 1,
        min.node.size = 1, replace = FALSE
      )

      # make predictions
      predictions <- predict(tree_model, data)$predictions

      # COMPUTE SWE
      pred_swe <- data$maxv_SNWD * predictions

      # Fit a LNORM distribution to the predictions
      lnorm_model <- fitdist(
        data = pred_swe, distr = "lnorm",
        start = list(
          meanlog = mean(log(pred_swe)), sdlog = stats::sd(log(pred_swe))
        ),
        method = "mle"
      )


      # Store the LNORM parameters in the matrix
      lnorm_params_matrix[i, ] <- lnorm_model$estimate
    }
    
  } else {
    
    # Fit different decision trees and make predictions
    for (i in 1:n_trees) {
      # Fit a single decision tree
      tree_model <- ranger(
        RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
          TD + ELEV,
        data = data, importance = "impurity", num.trees = 1,
        min.node.size = 1, replace = TRUE
      )

      # make predictions
      predictions <- predict(tree_model, data)$predictions

      # COMPUTE SWE
      pred_swe <- data$maxv_SNWD * predictions

      # Fit a LNORM distribution to the predictions
      lnorm_model <- fitdist(
        data = pred_swe, distr = "lnorm",
        start = list(
          meanlog = mean(log(pred_swe)), sdlog = stats::sd(log(pred_swe))
        ),
        method = "mle"
      )


      # Store the LNORM parameters in the matrix
      lnorm_params_matrix[i, ] <- lnorm_model$estimate
    }
  }

  return(lnorm_params_matrix)
}
