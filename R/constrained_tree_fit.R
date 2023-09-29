
constrained_tree_fit <- function(data, n_trees, lower.ratio = 0.2, higher.ratio = 0.5) {
  
  # required packages
  package_list <- c("matrixStats", "ranger", "fitdistrplus")
  lapply(package_list, require, character.only = TRUE)
  
  
  # set condition for constrained bootstrapping
  condition <- data$RATIO >= higher.ratio | data$RATIO <= lower.ratio
  
  
  # Define the indices of the data points to be kept in each synthetic sample
  keep_indices <- which(condition)
  
  #create empty list
  rf_trees <- list()
  
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
       replace = FALSE
    )
    
    rf_trees[[i]] <- tree_model
  }
  
  
  
  return(rf_trees)
}
