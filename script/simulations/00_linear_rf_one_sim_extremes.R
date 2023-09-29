#############################################################################
# This script is created to compare the actual versus estimated response 
#variable for a linear problem using a random forest mode. This is to 
# show that RF does not perform well at the extremes of the data. 
##############################################################################

##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries
# =============================================================================#
library(renv)
library(caret)
library(fitdistrplus)
library(distfixer)
library(ranger)
library(tidyverse)
source("R/simulate_gaussian_data.R")

set.seed(2343)

# =============================================================================#
# data creation
# =============================================================================#
data <- simulate_gaussian_data(sample_size = 10000, error_sd = 2)


# =============================================================================#
# data partition
# =============================================================================#

# Create a vector of row indices for splitting into train and test sets
train_indices <- caret::createDataPartition(data$y, p = 0.7, list = FALSE)

# Split the data into train and test sets
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


# =============================================================================#
# regression model
# =============================================================================#

model <- ranger(y ~ ., data = train_data)

train_data <- train_data %>%
  dplyr::mutate(
    pred_y = predict(model, data = train_data)$predictions,
    type = "Train"
  ) %>%
  dplyr::select(y, pred_y, type)

test_data <- test_data %>%
  dplyr::mutate(
    pred_y = predict(model, data = test_data)$predictions,
    type = "Test"
  ) %>%
  dplyr::select(y, pred_y, type)


df <- rbind(test_data, train_data)

# re-order factor levels
df$type <- factor(df$type,
  levels = c("Train", "Test")
)


ggplot(df, aes(y, pred_y)) +
  geom_point(color = "#b2182b", size = 2) +
  facet_grid(. ~ type) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black", lwd = 2) +
  ylab("Estimated Response") +
  xlab("Actual Response") +
  theme(
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text.x = element_text(size = 40, margin = margin(t = 10, b = 10)),
    axis.text.y = element_text(size = 40, margin = margin(r = 10, l = 10)),
    strip.text = element_text(size = 40)
  )
