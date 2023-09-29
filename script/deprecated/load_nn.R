
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
    logPPTWT = log(PPTWT), # Create a new column 'logPPTWT' with log of PPTWT column
    logSWE = log(maxv_WESD)
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
  dplyr::select(logSNWD, SMONTH, D2C, logPPTWT, MCMT, MWMT, TD, ELEV, logSWE)

test <- df %>%
  dplyr::filter(data == "Test") %>%
  dplyr::select(logSNWD, SMONTH, D2C, logPPTWT, MCMT, MWMT, TD, ELEV, logSWE)

# ===========================================================================#
# split features from labels
# ===========================================================================#
train_features = train %>% select(-logSWE)
test_features = test %>% select(-logSWE)

train_labels = train %>% select(logSWE)
test_labels = test %>% select(logSWE)

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

build_and_compile = function(norm){
  
}




















