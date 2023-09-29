
tree_gev_fitting <- function(data, n_trees, probs = 0.75, mean_para = TRUE) {
  # required packages
  package_list <- c("matrixStats", "ranger", "extRemes")
  lapply(package_list, require, character.only = TRUE)


  # Initialize a matrix to store the GEV parameters
  gev_params_matrix <- matrix(nrow = n_trees, ncol = 3)
  colnames(gev_params_matrix) <- c("location", "scale", "shape")


  # Fit different decision trees and make predictions
  for (i in 1:n_trees) {
    # Fit a single decision tree
    tree_model <- ranger(
      RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
        TD + ELEV,
      data = data, importance = "impurity", num.trees = 1
    )

    # make predictions
    predictions <- predict(tree_model, data)$predictions

    # COMPUTE SWE
    pred_swe <- data$maxv_SNWD * predictions

    # Fit a GEV distribution to the predictions
    gev_model <- extRemes::fevd(pred_swe, type = "GEV", method = "Lmoments")

    # Store the GEV parameters in the matrix
    gev_params_matrix[i, ] <- gev_model[["results"]]
  }


  # compute mean or quantile of the distr. parameters
  if (mean_para) {
    result <- colMeans(gev_params_matrix, na.rm = TRUE)
  } else {
    result <- colQuantiles(gev_params_matrix, probs = probs, na.rm = TRUE)
  }


  return(result)
}
