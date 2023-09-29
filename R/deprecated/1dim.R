

# Generate the independent variable and the error
x1 <- runif(n = 1000)


error <- rnorm(1000, 0, .1)

# Generate the dependent variable
y <-   x1 +   error

data <- data.frame(cbind(y, x1))



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

bias_reduction <- (abs(bias_adjusted - bias_unadjusted) / bias_unadjusted) * 100
bias_reduction
