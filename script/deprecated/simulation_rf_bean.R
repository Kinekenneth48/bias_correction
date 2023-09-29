

##############################################################################
# STEP 0: initial set up
##############################################################################

# =============================================================================#
# load libraries
# =============================================================================#
library(caret)
library(fitdistrplus)
library(ranger)
library(tidyverse)
library(MASS)



# =============================================================================#
# data creation
# =============================================================================#

# Set the mean and covariance for each feature
mu <- rep(0, 10)


# Set the covariance matrix with a range of 0 to 0.5
cov_range <- seq(0.0, 0.5, length.out = 10)
sigma <- diag(10) * 1.8
for (i in 1:10) {
  for (j in 1:10) {
    if (i != j) {
      sigma[i, j] <- cov_range[abs(i - j)]
    }
  }
}




# Generate 1000 samples from the multivariate normal distribution
data <- data.frame(mvrnorm(n = 10000, mu = mu, Sigma = sigma))

# create errror
error <- rnorm(10000, 0, 1) 

# create y by sum x's and add error
data$y <- rowSums(data[, 1:10]) + error






# =============================================================================#
# data partition
# =============================================================================#

# Create a vector of row indices for splitting into train and test sets
train_indices <- caret::createDataPartition(data$y, p = 0.7, list = FALSE)

# Split the data into train and test sets
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]




##############################################################################
# STEP 1: model development
##############################################################################


# =============================================================================#
# random forest model
# =============================================================================#
model_rf <- ranger(y ~ ., data = train_data)
model_lm <- lm(y ~ ., data = train_data)
model_rf
summary(model_lm)

pred_y_rf <- predict(model_rf, data = train_data)$predictions
pred_y_lm <- predict(model_lm, data = train_data)

plot(train_data$y, pred_y_rf)
abline(a = 0, b = 1, col = "red")

plot(train_data$y, pred_y_lm)
abline(a = 0, b = 1, col = "red")

fitdistrplus::fitdist(train_data$y, distr = "norm")
fitdistrplus::fitdist(pred_y_rf, distr = "norm")
fitdistrplus::fitdist(pred_y_lm, distr = "norm")

plot( test_data$y, pred_y)
abline(a = 0, b = 1, col = "red")

library(corrplot)

corrplot(cor(dat),
         method = "number",
         type = "upper" # show only upper side
)