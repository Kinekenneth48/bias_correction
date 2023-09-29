
# Use the individual decision tree models to create a random forest
combine_dt_to_rf_object <- function(dt_models) {
  # Create random forest object using decision tree models
  rf_object <- list(models = dt_models)
  
  return(rf_object)
}