
constrained_tree_lnorm_fitting <- function(data, n_trees, probs = 0.75, mean_para = TRUE,
                                           lower.ratio = 0.2, higher.ratio = 0.5, constrained_boot = TRUE) {
  # required packages
  package_list <- c("matrixStats", "ranger", "fitdistrplus")
  lapply(package_list, require, character.only = TRUE)


  # set condition for constrained bootstrapping
  condition <- data$RATIO >= higher.ratio | data$RATIO <= lower.ratio


  # Define the indices of the data points to be kept in each synthetic sample
  keep_indices <- which(condition)



  if (constrained_boot) {
    lnorm_params_matrix <- constrained_boot_fitting(data,
      keep_indices = keep_indices,
      n_trees = n_trees,
      constrained_boot = TRUE
    )
  } else {
    lnorm_params_matrix <- constrained_boot_fitting(data,
      n_trees = n_trees, 
      keep_indices = keep_indices, constrained_boot = FALSE
    )
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

return(lnorm_params_matrix)
  #return(result)
}
