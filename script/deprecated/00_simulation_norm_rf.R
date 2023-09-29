

##############################################################################
# STEP 0: initial set up
##############################################################################

# =============================================================================#
# load libraries
# =============================================================================#
library(caret)
library(fitdistrplus)
library(distfixer)
library(ranger)
library(tidyverse)
library(MASS)



# =============================================================================#
# data creation
# =============================================================================#

# Set the mean and covariance for each feature
mu <- rep(0, 10)


# Set the covariance matrix with a range of 0.3 to 0.7
cov_range <- seq(0.01, 0.3, length.out = 10)
sigma <- diag(10) * 1.8
for (i in 1:10) {
  for (j in 1:10) {
    if (i != j) {
      sigma[i, j] <- cov_range[abs(i - j)]
    }
  }
}




# Generate 1000 samples from the multivariate normal distribution
samples <- ((mvrnorm(n = 10000, mu = mu, Sigma = sigma)))

# convert to df
samples <- data.frame(samples)

# create errror
data <- samples
data$error <- rnorm(10000, 0, 0.9) # increasing sd increases bias reduction

# create y by sum x's and add error
data$y <- rowSums(data[, 1:10]) + data$error

# remove error
data <- data %>%
  dplyr::select(-error)
hist(data$y)




# =============================================================================#
# data partition
# =============================================================================#

# Create a vector of row indices for splitting into train and test sets
train_indices <- createDataPartition(data$y, p = 0.7, list = FALSE)

# Split the data into train and test sets
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]




##############################################################################
# STEP 1: model development
##############################################################################


# =============================================================================#
# random forest model
# =============================================================================#
model <- ranger(y ~ ., data = train_data)
model

pred_y <- predict(model, data = train_data)$predictions
plot(train_data$y, pred_y)
abline(a = 0, b = 1, col = "red")


fitdistrplus::fitdist(data = train_data$y, distr = "norm")

fitdistrplus::fitdist(data = pred_y, distr = "norm")


res <- as.numeric(train_data$y) - predict(model, train_data)[["predictions"]]
error_distr <- (fitdistrplus::fitdist(res,    distr = "norm"))

plot(error_distr)
# =============================================================================#
# error distr.
# =============================================================================#

# error
error <- distfixer::get_error_distribution(
  train_data = train_data, fitted_model = model,
  label = "y"
)

mean <- error[["estimate"]][["mean"]]
sd <- error[["estimate"]][["sd"]]



# =============================================================================#
# best percentile
# =============================================================================#

p <- distfixer::best_percentile(
  train_data = train_data, direct_label = "y", param_adjust = "sd",
  fitted_model = model, mean = mean, sd = sd, distr = "norm",
  label_convert = FALSE
)




##############################################################################
# STEP 2: model prediction
##############################################################################

# predict parameters for test data using distfixer
test_param_adjusted <- distfixer::predict_param(
  test_data = test_data, direct_label = "y", param_adjust = "sd",
  fitted_model = model, mean = mean, sd = sd, distr = "norm",
  label_convert = FALSE, all_missing = TRUE, percentile = p
)
test_param_adjusted

# predict parameters for actual y
test_param_actual <- fitdistrplus::fitdist(data = test_data$y, distr = "norm")
test_param_actual

# predict parameters for actual y
pred_y <- predict(model, data = test_data)$predictions
test_param_unadjusted <- fitdistrplus::fitdist(data = pred_y, distr = "norm")
test_param_unadjusted



bias_unadjusted_per <- ((test_param_unadjusted[["estimate"]][["sd"]] - test_param_actual[["estimate"]][["sd"]])
/ test_param_actual[["estimate"]][["sd"]]) * 100



bias_adjusted_per <- ((test_param_adjusted[["sd"]] - test_param_actual[["estimate"]][["sd"]])
/ test_param_actual[["estimate"]][["sd"]]) * 100

bias_unadjusted_per
bias_adjusted_per


bias_unadjusted <- (test_param_unadjusted[["estimate"]][["sd"]] - test_param_actual[["estimate"]][["sd"]])
bias_adjusted <- (test_param_adjusted[["sd"]] - test_param_actual[["estimate"]][["sd"]])

bias_reduction <- ((bias_adjusted - bias_unadjusted) / bias_unadjusted) * 100
bias_reduction
