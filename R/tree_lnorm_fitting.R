
tree_lnorm_fitting <- function(data, n_trees, probs = 0.75, mean_para = TRUE) {
  # required packages
  package_list <- c("matrixStats", "ranger", "fitdistrplus")
  lapply(package_list, require, character.only = TRUE)


  # Initialize a matrix to store the GEV parameters
  lnorm_params_matrix <- matrix(nrow = n_trees, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")


  # Fit different decision trees and make predictions
  for (i in 1:n_trees) {
    # Fit a single decision tree
    tree_model <- ranger(
      RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
        TD + ELEV,
      data = data, importance = "impurity", num.trees = 1,
      min.node.size = 1
    )

    # make predictions
    predictions <- predict(tree_model, data)$predictions

    # COMPUTE SWE
    pred_swe <- data$maxv_SNWD * predictions

    # Fit a GEV distribution to the predictions
    lnorm_model <- fitdist(
      data = pred_swe, distr = "lnorm",
      start = list(
        meanlog = mean(log(pred_swe)), sdlog = stats::sd(log(pred_swe))
      ),
      method = "mle"
    )


    # Store the GEV parameters in the matrix
    lnorm_params_matrix[i, ] <- lnorm_model$estimate
  }


  # compute mean or quantile of the distr. parameters
  if (mean_para) {
    result <- colMeans(lnorm_params_matrix, na.rm = TRUE)
  } else {
    result <- colQuantiles(lnorm_params_matrix, probs = probs, na.rm = TRUE)
  }


  return(result)
}
