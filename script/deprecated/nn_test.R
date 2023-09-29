
# ===========================================================================#
# load libraries and functions
# ===========================================================================#
library(ranger) # Load ranger library for Random Forest model
library(Metrics) # Load Metrics library for calculating performance metrics
library(tidyverse) # Load tidyverse library for data manipulation
library(ggplot2) # Load ggplot2  for visualization
library(reshape2) # Loadreshape2  for visualization and data manipulation
library(tensorflow)
library(keras)
library(tidymodels)
library(recipes)

use_virtualenv("r-reticulate")
tensorflow::tf_config()
reticulate::py_config()

# Source the R file
source(file = "R/fit_distribution.R")
source(file = "R/make_predictions.R")
source(file = "R/combine_dt_to_rf_object.R")

# ===========================================================================#
# load data
# ===========================================================================#
# Generate example data
set.seed(1)

df <- read.csv("data-raw/data.csv") # Read the data from data.csv file



df <- df %>%
  dplyr::mutate(
    logSNWD = log(maxv_SNWD), # Create a new column 'logSNWD' with log of maxv_SNWD column
    logPPTWT = log(PPTWT) # Create a new column 'logPPTWT' with log of PPTWT column
  ) %>%
  dplyr::rename(
    SMONTH = snow_month_SNWD, # Rename the column 'snow_month_SNWD' to 'SMONTH'
    D2C = dist2coast, # Rename the column 'dist2coast' to 'D2C'
    ELEV = ELEVATION, # Rename the column 'ELEVATION' to 'ELEV'
    RATIO = maxRatio # Rename the column 'maxRatio' to 'RATIO'
  )

#df$SMONTH <- as.factor(df$SMONTH)

train <- df %>%
  dplyr::filter(data == "Train") %>%
  dplyr::select(logSNWD, SMONTH, D2C, logPPTWT, MCMT, MWMT, TD, ELEV, maxv_WESD)

test <- df %>%
  dplyr::filter(data == "Test") %>%
  dplyr::select(logSNWD, SMONTH, D2C, logPPTWT, MCMT, MWMT, TD, ELEV, maxv_WESD)

# ===========================================================================#
# split features from labels
# ===========================================================================#
train_features = train %>% select(-maxv_WESD)
test_features = test %>% select(-maxv_WESD)

train_labels = train %>% select(maxv_WESD)
test_labels = test %>% select(maxv_WESD)

# ===========================================================================#
# normalize data
# ===========================================================================#

normalizer = keras::layer_normalization(axis = -1L)

normalizer %>% keras::adapt(as.matrix(train_features))

print(normalizer$mean)


first = as.matrix(train_features[1,])
cat('FIRST EXAMPLE', first)
cat('Normalized', as.matrix(normalizer(first)))



# ===========================================================================#
# BUILD NN 
# ===========================================================================#

# Define the log-normal likelihood function
log_likelihood <- function(y_true, y_pred) {
  loc <- y_pred[, 1]
  scale <- y_pred[, 2]
  x <- y_true
  log_density <-
  -log(x) - log(scale) - 0.5 * log(2 * pi) - 0.5 * ((log(x) - loc) / scale) ^ 2
  return(-keras$backend$mean(log_density))
}




# Test the log-normal likelihood function with some example data
y_true <- array(runif(100, 1, 10), dim = c(100, 1))
y_pred <- array(runif(200, 0, 1), dim = c(100, 2))
log_likelihood(y_true, y_pred)


#model definition


build_and_compile = function(norm){
  model = keras::keras_model_sequential()%>%
    norm()%>%
    keras::layer_dense(units = 64, activation = "relu")%>%
    keras::layer_dense(units = 64, activation = "relu")%>%
    keras::layer_dense(2) %>%
    layer_lambda(function(x) {
      loc <- keras$backend$softplus(x[, 1])
      scale <- keras$backend$softplus(x[, 2])
      return(keras$backend$stack(list(loc, scale), axis = 1))
    })
  
  model %>% keras::compile(
    loss = log_likelihood,
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = c("mae")
  )
  
  return(model)
}


# ===========================================================================#
# regression with dnn
# ===========================================================================#

dnn_model = build_and_compile(normalizer)
summary(dnn_model)


history = dnn_model %>%
  fit(
    as.matrix(train_features),
    as.matrix(train_labels),
    validation_split = 0.2,
    verbose = 0,
    epochs = 50
  )

plot(history)

#collect results on test

test_results = list()

test_results[['dnn_model']] = dnn_model %>%
  evaluate(
    as.matrix(test_features),
    as.matrix(test_labels),
    verbose = 0
  )

sapply(test_results, function(x) x)


#make predictions
test_predictions = predict(dnn_model , as.matrix(test_features))




ggplot(data.frame(pred = as.numeric(exp(test_predictions)), 
                  swe = exp(test_labels$logSWE)))+
  geom_point(aes(x = pred, y =swe))+
  geom_abline(intercept = 0, slope = 1, color = "red")




