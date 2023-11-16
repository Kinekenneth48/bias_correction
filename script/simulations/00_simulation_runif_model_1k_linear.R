#############################################################################
# This script is created to simulate data (1,000 obs. per sim) from a uniform model,
# fit to a linear, regression model, fit estimated response variable to a normal
# dist. and correct the bias in the spread parameter of the dist.
##############################################################################

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

set.seed(23553)
##############################################################################
# STEP 1: Generate 200 different data set and apply the bootstrap method
##############################################################################


bootstraps <- 200

# Initialize a matrix to store the parameters
param_adjusted_runif_1k <- matrix(nrow = bootstraps, ncol = 2)
colnames(param_adjusted_runif_1k) <- c("mean", "sd")


param_actual_runif_1k <- matrix(nrow = bootstraps, ncol = 2)
colnames(param_actual_runif_1k) <- c("mean", "sd")

param_unadjusted_runif_1k <- matrix(nrow = bootstraps, ncol = 2)
colnames(param_unadjusted_runif_1k) <- c("mean", "sd")

for (i in 1:bootstraps) {
  # Set a different seed for each bootstrap sample
  set.seed(i)


  # =============================================================================#
  # data creation
  # =============================================================================#


  # Generate the independent variable and the error
  x1 <- runif(n = 1000, min = -1, max = 1)
  x2 <- runif(n = 1000, min = -1, max = 1)
  x3 <- runif(n = 1000, min = -1, max = 1)
  x4 <- runif(n = 1000, min = -1, max = 1)
  x5 <- runif(n = 1000, min = -1, max = 1)

  error <- rnorm(1000, 0, 2)

  # Generate the dependent variable
  y <- -5 + 4 * x1 + 2.5 * (x2) + 5 * x3 + 2 * x4 + x5^2 + error



  data <- data.frame(cbind(y, x1, x2, x3, x4, x5))



  # =============================================================================#
  # data partition
  # =============================================================================#

  # Create a vector of row indices for splitting into train and test sets
  train_indices <- createDataPartition(data$y, p = 0.7, list = FALSE)

  # Split the data into train and test sets
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]

  # =============================================================================#
  # regression model
  # =============================================================================#

  model <- lm(y ~ ., data = train_data)


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

  param_adjusted_runif_1k[i, ] <- distfixer::predict_param(
    test_data = test_data, direct_label = "y", param_adjust = "sd",
    fitted_model = model, mean = mean, sd = sd, distr = "norm",
    label_convert = FALSE, all_missing = TRUE, percentile = p
  )

  # =============================================================================#
  # get actual parameter
  # =============================================================================#

  param_actual_runif_1k[i, ] <- fitdistrplus::fitdist(
    data = test_data$y,
    distr = "norm"
  )[["estimate"]]


  # =============================================================================#
  # get unadjusted parameter
  # =============================================================================#
  pred_y <- predict(model, data = test_data)

  param_unadjusted_runif_1k[i, ] <- fitdistrplus::fitdist(
    data = pred_y,
    distr = "norm"
  )[["estimate"]]
}




# =============================================================================#
# save data for use later
# =============================================================================#

save(param_unadjusted_runif_1k, file = "data-raw/RObject/param_unadjusted_runif_1k.RData")
save(param_actual_runif_1k, file = "data-raw/RObject/param_actual_runif_1k.RData")
save(param_adjusted_runif_1k, file = "data-raw/RObject/param_adjusted_runif_1k.RData")




##############################################################################
# STEP 2: Create a graph to display the bias correction impact
##############################################################################

base::load("data-raw/RObject/param_unadjusted_runif_1k.RData")
base::load("data-raw/RObject/param_actual_runif_1k.RData")
base::load("data-raw/RObject/param_adjusted_runif_1k.RData")


# convert to data frame

param_unadjusted_runif_1k <- as.data.frame(param_unadjusted_runif_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_unadjusted = mean, sd_unadjusted = sd) %>%
  dplyr::select(id, mean_unadjusted, sd_unadjusted)


param_adjusted_runif_1k <- as.data.frame(param_adjusted_runif_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_adjusted = mean, sd_adjusted = sd) %>%
  dplyr::select(id, mean_adjusted, sd_adjusted)


param_actual_runif_1k <- as.data.frame(param_actual_runif_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_runif_1k <- inner_join(
  x = param_actual_runif_1k, y = param_adjusted_runif_1k, by = c("id")
)

comb_para_runif_1k <- inner_join(
  x = comb_para_runif_1k, y = param_unadjusted_runif_1k, by = c("id")
)





# create relative ratios
comb_para_runif_1k <- comb_para_runif_1k %>%
  dplyr::mutate(
    ratio_adjust_mean = mean_adjusted / mean,
    ratio_adjust_sd = sd_adjusted / sd,
    ratio_unadjust_mean = mean_unadjusted / mean,
    ratio_unadjust_sd = sd_unadjusted / sd
  )

# compute bias and cov of ratios
comb_para_runif_1k <- comb_para_runif_1k %>%
  dplyr::mutate(
    bias_mean_adj = mean(ratio_adjust_mean),
    bias_sd_adj = mean(ratio_adjust_sd),
    bias_mean_unadj = mean(ratio_unadjust_mean),
    bias_sd_unadj = mean(ratio_unadjust_sd),
    cov_adjust_mean = (sd(ratio_adjust_mean) / bias_mean_adj),
    cov_adjust_sd = (sd(ratio_adjust_sd) / bias_sd_adj),
    cov_unadjust_mean = (sd(ratio_unadjust_mean) / bias_mean_unadj),
    cov_unadjust_sd = (sd(ratio_unadjust_sd) / bias_sd_unadj)
  )



# Calculate the mean of each column
column_means <- colMeans(comb_para_runif_1k)

# Convert the named vector to a dataframe and transpose it
column_means_df <- as.data.frame(t(column_means))



data_long <- comb_para_runif_1k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "ratio"
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


ggplot(data_long, aes(x = Measure, y = ratio, fill = Measure)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.85, 1.15)) +
  ggtitle("Boxplot comparing the relative parameter ratio for the UNIFORM model") +
  theme(
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    strip.text = element_text(size = 25)
  )
