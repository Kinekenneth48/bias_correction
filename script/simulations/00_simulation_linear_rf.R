#############################################################################
# This script is created to compare the ratio of the normal distribution 
#parameters of the actual vs. estimated response variable when bias is corrected 
#or not for a linear problem when RF model is used.This script
# is the follow-up of 00_linear_rf_one_sim_extremes.R script
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



##############################################################################
# STEP 1: Generate 200 different data set and apply the bootstrap method
##############################################################################

bootstraps <- 200

# Initialize a matrix to store the parameters
param_adjusted_gaussian_rf <- matrix(nrow = bootstraps, ncol = 2)
colnames(param_adjusted_gaussian_rf) <- c("mean", "sd")


param_actual_gaussian_rf <- matrix(nrow = bootstraps, ncol = 2)
colnames(param_actual_gaussian_rf) <- c("mean", "sd")

param_unadjusted_gaussian_rf <- matrix(nrow = bootstraps, ncol = 2)
colnames(param_unadjusted_gaussian_rf) <- c("mean", "sd")


for (i in 1:bootstraps) {
  # Set a different seed for each bootstrap sample
  set.seed(i)
  
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
  
  model <-  ranger(y ~ ., data = train_data)
  
  # =============================================================================#
  # error distr.
  # =============================================================================#
  
  # error
  error <- distfixer::get_error_distribution(
    train_data = train_data,
    fitted_model = model, label = "y"
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
  
  
  
  
  # =============================================================================#
  # get adjusted parameter
  # =============================================================================#
  
  param_adjusted_gaussian_rf[i, ] <- distfixer::predict_param(
    test_data = test_data, direct_label = "y", param_adjust = "sd",
    fitted_model = model, mean = mean, sd = sd, distr = "norm",
    label_convert = FALSE, all_missing = TRUE, percentile = p
  )
  
  # =============================================================================#
  # get actual parameter
  # =============================================================================#
  
  param_actual_gaussian_rf[i, ] <- fitdistrplus::fitdist(
    data = test_data$y,
    distr = "norm"
  )[["estimate"]]
  
  
  # =============================================================================#
  # get unadjusted parameter
  # =============================================================================#
  pred_y <- predict(model, data = test_data)$predictions
  
  param_unadjusted_gaussian_rf[i, ] <- fitdistrplus::fitdist(
    data = pred_y,
    distr = "norm"
  )[["estimate"]]
}

# =============================================================================#
# save data for use later
# =============================================================================#

save(param_unadjusted_gaussian_rf, file = "data-raw/RObject/param_unadjusted_gaussian_rf.RData")
save(param_actual_gaussian_rf, file = "data-raw/RObject/param_actual_gaussian_rf.RData")
save(param_adjusted_gaussian_rf, file = "data-raw/RObject/param_adjusted_gaussian_rf.RData")








##############################################################################
# STEP 2: Create a graph to display the bias correction impact
##############################################################################

load("data-raw/RObject/param_unadjusted_gaussian_rf.RData")
load("data-raw/RObject/param_actual_gaussian_rf.RData")
load("data-raw/RObject/param_adjusted_gaussian_rf.RData")



# convert to data frame

param_unadjusted_gaussian_rf <- as.data.frame(param_unadjusted_gaussian_rf) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_unadjusted = mean, sd_unadjusted = sd) %>%
  dplyr::select(id, mean_unadjusted, sd_unadjusted)


param_adjusted_gaussian_rf <- as.data.frame(param_adjusted_gaussian_rf) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_adjusted = mean, sd_adjusted = sd) %>%
  dplyr::select(id, mean_adjusted, sd_adjusted)


param_actual_gaussian_rf <- as.data.frame(param_actual_gaussian_rf) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_gaussian <- inner_join(
  x = param_actual_gaussian_rf, y = param_adjusted_gaussian_rf, by = c("id")
)

comb_para_gaussian <- inner_join(
  x = comb_para_gaussian, y = param_unadjusted_gaussian_rf, by = c("id")
)




# create relative ratios
comb_para_gaussian <- comb_para_gaussian %>%
  dplyr::mutate(
    ratio_adjust_mean = mean_adjusted / mean,
    ratio_adjust_sd = sd_adjusted / sd,
    ratio_unadjust_mean = mean_unadjusted / mean,
    ratio_unadjust_sd = sd_unadjusted / sd
  )




data_long <- comb_para_gaussian %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long <- data_long %>%
  mutate(method = case_when(
    Measure == "ratio_adjust_mean" ~ "Adjusted Parameters",
    Measure == "ratio_adjust_sd" ~ "Adjusted Parameters",
    Measure == "ratio_unadjust_mean" ~ "Unadjusted Parameters",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted Parameters"
  ))



data_long <- data_long %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))


data_long$method <- as.factor(data_long$method)
levels(data_long$method)


# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#

# re-order factor levels
data_long$method <- factor(data_long$method,
                         levels = c("Unadjusted Parameters", "Adjusted Parameters")
)


ggplot(data_long, aes(x = Measure, y = Ratio)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
 ylim(c(0.7, 1.1)) +
  ggtitle("Boxplot comparing the relative parameter ratio for the Gaussian model")  +
  scale_x_discrete(labels = c("mean" = expression(mu), "sd" = expression(sigma))) +
  theme(
    legend.position = c(0.85, 0.9),
    legend.direction = "horizontal",
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text.x = element_text(size = 40, margin = margin(t = 10, b = 10)), 
    axis.text.y = element_text(size = 40, margin = margin(r = 10, l = 10)), 
    strip.text = element_text(size = 40)
  )
