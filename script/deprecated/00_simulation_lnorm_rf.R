

##############################################################################
# STEP 0: initial set up
##############################################################################

# =============================================================================#
# load libraries
# =============================================================================#
library(caret)
library(renv)
library(fitdistrplus)
library(distfixer)
library(ranger)
library(tidyverse)
library(MASS)



# =============================================================================#
# data creation
# =============================================================================#

# Set the mean and covariance for each feature
 mu <- rnorm(10, mean = 5, sd = 5)



# Set the covariance matrix with a range of 0.3 to 0.7
cov_range <- seq(0.0, 0.6, length.out = 10)
sigma <- diag(10) * 1.8
for (i in 1:10) {
  for (j in 1:10) {
    if (i != j) {
      sigma[i, j] <- cov_range[abs(i - j)]
    }
  }
}



# Generate 1000 samples from the multivariate normal distribution
samples <- (((mvrnorm(n = 10000, mu = mu, Sigma = sigma)))) / 10

# convert to df
samples <- data.frame(samples)

# create error
data <- samples
data$error <- rnorm(10000, 0, 0.5)

# create y by sum x's and add error
data$y <- exp(rowSums(data[, 1:10])) + exp(data$error)

# remove error
data <- data %>%
  dplyr::select(-error)
hist(data$y)

descdist(data$y, discrete = FALSE, boot = 200)
fit.norm <- fitdist(data$y, "lnorm")
plot(fit.norm)

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
descdist(res, discrete = FALSE, boot = 200)
error_distr <- (fitdistrplus::fitdist(res, distr = "norm"))
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
  train_data = train_data, direct_label = "y", param_adjust = "sdlog", nboot = 200,
  fitted_model = model, mean = mean, sd = sd, distr = "lnorm", bounds = c(0.001, Inf),
  label_convert = FALSE, residuals = TRUE, residual_sample = res)




##############################################################################
# STEP 2: model prediction
##############################################################################

# predict parameters for test data using distfixer
test_param_adjusted <- distfixer::predict_param(
  test_data = test_data, direct_label = "y", param_adjust = "sdlog", nboot = 200,
  fitted_model = model, mean = mean, sd = sd, distr = "lnorm", bounds = c(0.1, Inf),
  label_convert = FALSE, all_missing = TRUE, percentile = p, residuals = FALSE, 
  residual_sample = res
)
test_param_adjusted

# predict parameters for actual y
test_param_actual <- fitdistrplus::fitdist(data = test_data$y, distr = "lnorm")
test_param_actual

# predict parameters for actual y
pred_y <- predict(model, data = test_data)$predictions
test_param_unadjusted <- fitdistrplus::fitdist(data = pred_y, distr = "lnorm")
test_param_unadjusted



bias_unadjusted_per <- ((test_param_unadjusted[["estimate"]][["sdlog"]] - test_param_actual[["estimate"]][["sdlog"]])
/ test_param_actual[["estimate"]][["sdlog"]]) * 100



bias_adjusted_per <- ((test_param_adjusted[["sdlog"]] - test_param_actual[["estimate"]][["sdlog"]])
/ test_param_actual[["estimate"]][["sdlog"]]) * 100

bias_unadjusted_per
bias_adjusted_per


bias_unadjusted <- (test_param_unadjusted[["estimate"]][["sdlog"]] - test_param_actual[["estimate"]][["sdlog"]])
bias_adjusted <- (test_param_adjusted[["sdlog"]] - test_param_actual[["estimate"]][["sdlog"]])

bias_reduction <- (( bias_unadjusted-bias_adjusted ) / bias_unadjusted) * 100
bias_reduction
