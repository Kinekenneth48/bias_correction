
# ===========================================================================#
# load libraries and functions
# ===========================================================================#
library(ranger)
library(Metrics)
library(renv)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(fitdistrplus)

# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/make_predictions.R")
source(file = "R/combine_dt_to_rf_object.R")

# ===========================================================================#
# load data
# ===========================================================================#

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






# ===========================================================================#
## Fit initial random forest model
# ===========================================================================#

rf1 <- ranger(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV, # Define the predictor variables and target variable
  data = df, importance = "impurity",
  num.trees = 100, keep.inbag = TRUE # Fit a random forest model with 100 trees and save in-bag observations
)

# Generate out-of-bag residuals
residuals <- df$RATIO - rf1$predictions

# full data prediction
x_data <- df %>%
  dplyr::select(
    logSNWD, SMONTH, D2C, logPPTWT, MCMT, MWMT,
    TD, ELEV
  )

full_data_pred <- predict(rf1, x_data)$predictions




# ===========================================================================#
## Residual bootstrap
# ===========================================================================#
# number of bootstrap iteration
n_iter <- 1000

# holder for the bootstrap trees
bootstrap_trees <- list()

for (i in 1:n_iter) {
  # Perform residual bootstrapping
  # Sample from the residuals with replacement
  bootstrap_samples <- matrix(sample(residuals,
    replace = TRUE,
    size = length(residuals)
  ), ncol = 1)

  # Add the bootstrapped residuals to the full data predictions
  y_bootstrap <- full_data_pred + bootstrap_samples

  # Fit a decision tree model using the bootstrapped data
  rf_temp <- ranger(
    y_bootstrap ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT +
      MWMT + TD + ELEV,
    data = x_data,
    importance = "impurity", num.trees = 1 # Fit a single decision tree
  )

  bootstrap_trees[[i]] <- rf_temp # Store the decision tree in the list
}






# ===========================================================================#
## make predictions
# ===========================================================================#

# Create the random forest object
rf_object <- combine_dt_to_rf_object(bootstrap_trees)



# Make predictions using the random forest
re_pred <- make_predictions(rf_object, x_data)


# calculate the mean squared error for the predictions
Metrics::mse(df$RATIO, full_data_pred)

# calculate the mean squared error for the bias corrected predictions
Metrics::mse(df$RATIO, 2 * full_data_pred - re_pred)

# calculate the relative improvement in MSE
(Metrics::mse(df$RATIO, 2 * full_data_pred - re_pred) -
  Metrics::mse(df$RATIO, full_data_pred)) / Metrics::mse(df$RATIO, full_data_pred)





# Create scatter plot
par(mfrow = c(1, 2))

# plot the full data predictions
plot(df$RATIO, full_data_pred,
  xlim = c(0, 1), ylim = c(0, 1),
  main = "Original ratio vs predicted ratio"
)
abline(a = 0, b = 1, col = "blue") # add a line with y=x


plot(df$RATIO, 2 * full_data_pred - re_pred,
  xlim = c(0, 1), ylim = c(0, 1),
  main = "Original ratio vs inflated residual bootstrap RF ratio"
)
# plot the bias corrected predictions
abline(a = 0, b = 1, col = "blue") # add a line with y=x



# Create new columns in the dataframe with the bias corrected and uncorrected SWE values
df <- df %>%
  mutate(
    bias_corrected_SWE = (2 * full_data_pred - re_pred) * maxv_SNWD,
    bias_uncorrected_SWE = full_data_pred * maxv_SNWD
  )






##############################################################################
# STEP 2: Compute relative parameter ratios for converted loads
##############################################################################

# ===========================================================================#
# filter stations
# ===========================================================================#
# Filter stations with annual max more than 30
id <- df %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 30)

df <- df %>%
  dplyr::filter(ID %in% id$ID)



# Convert dataframe of data into list by ID
ls_df <- df %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df$ID))


# Fit distribution of maxv_WESD and bias corrected and uncorrected SWE values
direct_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "maxv_WESD"
)) %>%
  mutate(
    mean = exp(meanlog),
    sd = exp(sdlog)
  )

uncorrected_con_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "bias_uncorrected_SWE"
)) %>%
  mutate(
    uc_mean = exp(meanlog),
    uc_sd = exp(sdlog)
  )



corrected_con_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "bias_corrected_SWE"
)) %>%
  mutate(
    c_mean = exp(meanlog),
    c_sd = exp(sdlog)
  )



# combine distr. para into one dataframe
comb_para_lnorm <- inner_join(direct_load_para_lnorm, corrected_con_load_para_lnorm, by = c("ID"))
comb_para_lnorm <- inner_join(comb_para_lnorm, uncorrected_con_load_para_lnorm, by = c("ID"))

comb_para_lnorm <- comb_para_lnorm %>%
  mutate(
    ratio_mean_uc = uc_mean / mean,
    ratio_sd_uc = uc_sd / sd,
    ratio_mean_c = c_mean / mean,
    ratio_sd_c = c_sd / sd
  )




data_long <- comb_para_lnorm %>% pivot_longer(
  cols = c(
    "ratio_mean_uc", "ratio_sd_uc",
    "ratio_mean_c", "ratio_sd_c"
  ),
  names_to = "variable", values_to = "value"
)


data_long <- data_long %>%
  mutate(method = case_when(
    variable == "ratio_mean_uc" ~ "uncorrected",
    variable == "ratio_sd_uc" ~ "uncorrected",
    variable == "ratio_mean_c" ~ "corrected",
    variable == "ratio_sd_c" ~ "corrected"
  ))


data_long <- data_long %>%
  mutate(variable = case_when(
    variable == "ratio_mean_uc" ~ "mean",
    variable == "ratio_mean_c" ~ "mean",
    variable == "ratio_sd_uc" ~ "sd",
    variable == "ratio_sd_c" ~ "sd"
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
