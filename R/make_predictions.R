make_predictions <- function(dt_models, new_data) {
  # Initialize empty vector to store predictions
  predictions <- numeric(nrow(new_data))
  
  # Loop through decision tree models and make predictions
  for (i in 1:length(dt_models[["models"]])) {
    predictions <- predictions + predict(dt_models[["models"]][[i]], new_data,
                                         type = "response"
    )[["predictions"]]
  }
  
  # Average predictions
  predictions <- predictions / length(dt_models[["models"]])
  
  return(predictions)
}