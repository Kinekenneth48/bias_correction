
# ===========================================================================#
# load libraries and functions
# ===========================================================================#
library(renv)
library(ranger) # Load ranger library for Random Forest model
library(Metrics) # Load Metrics library for calculating performance metrics
library(tidyverse) # Load tidyverse library for data manipulation
library(ggplot2)  # Load ggplot2  for visualization 
library(reshape2)# Loadreshape2  for visualization and data manipulation


#Source the R file
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
    logPPTWT = log(PPTWT) # Create a new column 'logPPTWT' with log of PPTWT column
  ) %>%
  dplyr::rename(
    SMONTH = snow_month_SNWD, # Rename the column 'snow_month_SNWD' to 'SMONTH'
    D2C = dist2coast, # Rename the column 'dist2coast' to 'D2C'
    ELEV = ELEVATION, # Rename the column 'ELEVATION' to 'ELEV'
    RATIO = maxRatio # Rename the column 'maxRatio' to 'RATIO'
  )


# train <- df %>%
#   dplyr::filter(data == "Train")
#
# test <- df %>%
#   dplyr::filter(data == "Test")





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


# calculate the mean absolute error for the predictions
Metrics::mae(df$RATIO, full_data_pred) 

# calculate the mean absolute error for the bias corrected predictions
Metrics::mae(df$RATIO, 2 * full_data_pred - re_pred) 



# Create scatter plot 
par(mfrow = c(1, 2))

# plot the full data predictions
plot(df$RATIO, full_data_pred, xlim = c(0, 1), ylim = c(0, 1), 
     main = "Original ratio vs predicted ratio") 
abline(a = 0, b = 1, col = "blue") # add a line with y=x


plot(df$RATIO, 2 * full_data_pred - re_pred, xlim = c(0, 1), ylim = c(0, 1),
     main = "Original ratio vs inflated residual bootstrap RF ratio")
# plot the bias corrected predictions
abline(a = 0, b = 1, col = "blue") # add a line with y=x



# Create new columns in the dataframe with the bias corrected and uncorrected SWE values
df <- df %>%
  mutate(
    bias_corrected_SWE = (2 * full_data_pred - re_pred) * maxv_SNWD,
    bias_uncorrected_SWE = full_data_pred  * maxv_SNWD
  )






##############################################################################
# STEP 2: Compute relative parameter ratios for converted loads
##############################################################################

# ===========================================================================#
# filter stations
# ===========================================================================#
# Filter stations with annual max more than 30
id <- df %>%
  count(ID) %>%
  filter(n >= 30)

df <- df %>%
  filter(ID %in% id$ID)



# Convert dataframe of data into list by ID
ls_df <- df %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df$ID))


# Fit distribution of maxv_WESD and bias corrected and uncorrected SWE values
direct_load_para <- do.call(rbind, lapply(ls_df,
  FUN = fit_distribution,
  "maxv_WESD",
  dist_name = "GEV"
))

uncorrected_con_load_para <- do.call(rbind, lapply(ls_df,
  FUN = fit_distribution,
  "bias_uncorrected_SWE",
  dist_name = "GEV"
)) %>%
  rename(
    UC_LOC_PRED = LOC_PRED,
    UC_SCALE_PRED = SCALE_PRED,
    UC_SHAPE_PRED = SHAPE_PRED
  )



corrected_con_load_para <- do.call(rbind, lapply(ls_df,
  FUN = fit_distribution,
  "bias_corrected_SWE",
  dist_name = "GEV"
)) %>%
  rename(
    C_LOC_PRED = LOC_PRED,
    C_SCALE_PRED = SCALE_PRED,
    C_SHAPE_PRED = SHAPE_PRED
  )



# combine distr. para into one dataframe
comb_para <- inner_join(direct_load_para, corrected_con_load_para, by = c("ID"))
comb_para <- inner_join(comb_para, uncorrected_con_load_para, by = c("ID"))

comb_para <- comb_para %>%
  mutate(
    ratio_loc_uc = UC_LOC_PRED / LOC_PRED,
    ratio_scale_uc = UC_SCALE_PRED / SCALE_PRED,
    ratio_shape_uc = UC_SHAPE_PRED / SHAPE_PRED,
    ratio_loc_c = C_LOC_PRED / LOC_PRED,
    ratio_scale_c = C_SCALE_PRED / SCALE_PRED,
    ratio_shape_c = C_SHAPE_PRED / SHAPE_PRED
  )




data_long <- comb_para %>% pivot_longer(
  cols = c(
    "ratio_loc_uc", "ratio_scale_uc",
    "ratio_shape_uc", "ratio_loc_c", "ratio_scale_c", "ratio_shape_c"
  ),
  names_to = "variable", values_to = "value"
)


data_long <- data_long %>%
  mutate(method = case_when(
    variable == "ratio_loc_uc" ~ "uncorrected",
    variable == "ratio_scale_uc" ~ "uncorrected",
    variable == "ratio_shape_uc" ~ "uncorrected",
    variable == "ratio_loc_c" ~ "corrected",
    variable == "ratio_scale_c" ~ "corrected",
    variable == "ratio_shape_c" ~ "corrected"
  ))


data_long <- data_long %>%
  mutate(variable = case_when(
    variable == "ratio_loc_uc" ~ "loc",
    variable == "ratio_scale_uc" ~ "scale",
    variable == "ratio_shape_uc" ~ "shape",
    variable == "ratio_loc_c" ~ "loc",
    variable == "ratio_scale_c" ~ "scale",
    variable == "ratio_shape_c" ~ "shape"
  ))

# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(-2,2))+
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load(using inflated residual bootstrapping)")


