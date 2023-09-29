
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
library(tfdatasets)

# TensorFlow v2.11.0 (~/.virtualenvs/r-reticulate/lib/python3.8/site-packages/tensorflow)
# Python v3.8 (~/.virtualenvs/r-reticulate/bin/python)

use_virtualenv("r-reticulate")
tensorflow::tf_config()
reticulate::py_config()

# Source the R file
source(file = "R/fit_distribution.R")
source(file = "R/make_predictions.R")
source(file = "R/combine_dt_to_rf_object.R")
source(file = "R/fit_lnorm.R")

# ===========================================================================#
# load data
# ===========================================================================#

df <- read.csv("data-raw/data.csv") # Read the data from data.csv file



df <- df %>%
  # dplyr::mutate(
  #  # logSNWD = log(maxv_SNWD), # Create a new column 'logSNWD' with log of maxv_SNWD column
  #   logPPTWT = log(PPTWT)# Create a new column 'logPPTWT' with log of PPTWT column
  # ) %>%
  dplyr::rename(
    SMONTH = snow_month_SNWD, # Rename the column 'snow_month_SNWD' to 'SMONTH'
    D2C = dist2coast, # Rename the column 'dist2coast' to 'D2C'
    ELEV = ELEVATION, # Rename the column 'ELEVATION' to 'ELEV'
    RATIO = maxRatio # Rename the column 'maxRatio' to 'RATIO'
  )

df_nn <- df %>%
  # dplyr::filter(ID == "11E09:ID:SNOW")%>%
  dplyr::select(
    maxv_SNWD, SMONTH, D2C, PPTWT, MCMT, MWMT, TD, ELEV, maxv_WESD,
    LONGITUDE, LATITUDE
  )


#define lat and long boundaries
min_lat = 24.6
max_lat = 49.4
min_lon = -124.8
max_lon = -66.9

#compute the spacing between the grid points in degrees (1km or 0.01 degrees per grid points)
#spacing  = 0.01/111
spacing  = 0.005/111

#define the lat and lon of the grids
lats = seq(from = min_lat, to = max_lat, by = spacing)
lons = seq(from = min_lon, to = max_lon, by = spacing)

#print no of grid points
cat("no of lat points:", length(lats), "\n")
cat("no of lon points:", length(lons), "\n")

unique_lat_long <- unique(df[, c("LATITUDE", "LONGITUDE")])
num_unique <- nrow(unique_lat_long)

spec = tfdatasets::feature_spec(df_nn, maxv_WESD ~ maxv_SNWD + SMONTH + D2C +
     PPTWT + MCMT + MWMT + TD + ELEV + LONGITUDE + LATITUDE)%>%
  tfdatasets::step_numeric_column(maxv_SNWD,D2C, PPTWT, MCMT, MWMT, TD, ELEV,
    normalizer_fn = scaler_standard()
  ) %>%
  tfdatasets::step_numeric_column(SMONTH) %>%
  tfdatasets::step_numeric_column( LONGITUDE, LATITUDE) %>%
  tfdatasets::step_bucketized_column(LONGITUDE, boundaries = c(lons)) %>%
  tfdatasets::step_bucketized_column(LATITUDE, boundaries = c(lats)) %>%
  tfdatasets::step_crossed_column(LATITUDE_LONGITUDE =
                  c(bucketized_LATITUDE, bucketized_LONGITUDE),
                                 hash_bucket_size = 10000000)



spec = fit(spec)

input = layer_input_from_dataset(df_nn %>% select(-maxv_WESD))

output = input %>%
  layer_dense_features(feature_columns = dense_features(spec))%>%
  keras::layer_dense(units = 10, activation = "relu") %>%
  keras::layer_dense(2) %>%
  layer_lambda(function(x) {
    loc <- keras$backend$softplus(x[, 1])
    scale <- keras$backend$softplus(x[, 2])
    return(keras$backend$stack(list(loc, scale), axis = 1))
  })


model = keras_model(input, output)

model %>% keras::compile(
  loss = log_likelihood,
  optimizer = optimizer_adam(learning_rate = 0.001)
)

history <-  model %>% 
  fit(
    x = df_nn %>% select(-maxv_WESD),
    y = df_nn$maxv_WESD,
    validation_split = 0.2,
    verbose = 0,
    epochs = 15
  )


plot(history)

# collect results on test

test_results <- list()

test_results[["dnn_model"]] <- model %>%
  evaluate(
    x = df_nn %>% select(-maxv_WESD),
    y = df_nn$maxv_WESD,
    verbose = 0
  )

sapply(test_results, function(x) x)


# make predictions
df$predictions <- exp(predict(model, df_nn %>% select(-maxv_WESD)))









# ===========================================================================#
# split features from labels
# ===========================================================================#

df_features <- df_nn %>% dplyr::select(-maxv_WESD)
df_labels <- df_nn %>% dplyr::select(maxv_WESD)



# ===========================================================================#
# normalize data
# ===========================================================================#

normalizer <- keras::layer_normalization(axis = -1L)

normalizer %>% keras::adapt(as.matrix(df_features))

print(normalizer$mean)



# ===========================================================================#
# BUILD NN
# ===========================================================================#

# Define the log-normal likelihood function
log_likelihood <- function(y_true, y_pred) {
  loc <- y_pred[, 1]
  scale <- y_pred[, 2]
  x <- y_true
  log_density <-
    -log(x) - log(scale) - 0.5 * log(2 * pi) - 0.5 * ((log(x) - loc) / scale)^2

  return(-keras$backend$mean(log_density))
}


build_and_compile <- function(norm) {


  model <- keras::keras_model_sequential() %>%
    norm() %>%
    keras::layer_dense(units = 10, activation = "relu") %>%
    keras::layer_dense(2) %>%
    layer_lambda(function(x) {
      loc <- keras$backend$softplus(x[, 1])
      scale <- keras$backend$softplus(x[, 2])
      return(keras$backend$stack(list(loc, scale), axis = 1))
    })

  model %>% keras::compile(
    loss = log_likelihood,
    optimizer = optimizer_adam(learning_rate = 0.001)
  )

  return(model)
}





# ===========================================================================#
# regression with dnn
# ===========================================================================#

dnn_model <- build_and_compile(normalizer)
summary(dnn_model)


history <- dnn_model %>%
  fit(
    as.matrix(df_features),
    as.matrix(df_labels),
    validation_split = 0.2,
    verbose = 0,
    epochs = 700
  )

plot(history)

# collect results on test

test_results <- list()

test_results[["dnn_model"]] <- dnn_model %>%
  evaluate(
    as.matrix(df_features),
    as.matrix(df_labels),
    verbose = 0
  )

sapply(test_results, function(x) x)


# make predictions
df$predictions <- exp(predict(dnn_model, as.matrix(df_features)))




ggplot(data.frame(
  pred = exp(df$predictions),
  swe = exp(df_labels$logSWE)
)) +
  geom_point(aes(x = pred, y = swe)) +
  geom_abline(intercept = 0, slope = 1, color = "red")



df <- df %>%
  mutate(
    pred = exp(predictions),
    SWE = exp(logSWE)
  )
#############################################################



# Filter stations with annual max more than 30
id <- df %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 30)

df <- df %>%
  dplyr::filter(ID %in% id$ID)

df$pred <- as.numeric(df$pred)

# Convert dataframe of data into list by ID
ls_df <- df %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df$ID))



# Fit distribution of maxv_WESD and bias corrected and uncorrected SWE values
direct_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "SWE"
)) %>%
  mutate(
    mean = exp(meanlog),
    sd = exp(sdlog)
  )


pred_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "pred"
)) %>%
  mutate(
    nn_mean = exp(meanlog),
    nn_sd = exp(sdlog)
  )




# combine distr. para into one dataframe
comb_para_lnorm <- inner_join(direct_load_para_lnorm, pred_load_para_lnorm, by = c("ID"))

comb_para_lnorm <- comb_para_lnorm %>%
  mutate(
    ratio_mean_nn = nn_mean / mean,
    ratio_sd_nn = nn_sd / sd
  )




data_long <- comb_para_lnorm %>% pivot_longer(
  cols = c(
    "ratio_mean_nn", "ratio_sd_nn"
  ),
  names_to = "variable", values_to = "value"
)


data_long <- data_long %>%
  mutate(method = case_when(
    variable == "ratio_mean_nn" ~ "neuralnetwork",
    variable == "ratio_sd_nn" ~ "neuralnetwork"
  ))


data_long <- data_long %>%
  mutate(variable = case_when(
    variable == "ratio_mean_nn" ~ "mean",
    variable == "ratio_sd_nn" ~ "sd"
  ))

# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.6)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load(using inflated residual bootstrapping)")
