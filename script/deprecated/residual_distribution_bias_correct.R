

# ===========================================================================#
# load libraries and functions
# ===========================================================================#
library(ranger) # load the ranger library for fitting random forest models
library(Metrics) # load the Metrics library for performance evaluation
library(tidyverse) # load the tidyverse library for data manipulation and visualization
library(ggplot2) # load the ggplot2 library for data visualization
library(reshape2) # load the reshape2 library for reshaping data

# Source the R file
source(file = "R/fit_distribution.R") #  for fitting distributions
source(file = "R/make_predictions.R") # for making predictions
source(file = "R/combine_dt_to_rf_object.R") #  for combining decision trees into RF objects


# ===========================================================================#
# load data
# ===========================================================================#
# load the data file and assign it to the variable df
df <- read.csv("data-raw/data.csv")

# Perform data cleaning and preprocessing
df <- df %>%
  dplyr::mutate(
    logSNWD = log(maxv_SNWD),
    logPPTWT = log(PPTWT)
  ) %>%
  dplyr::rename(
    SMONTH = snow_month_SNWD,
    D2C = dist2coast,
    ELEV = ELEVATION,
    RATIO = maxRatio
  ) %>%
  mutate(unique_id = row_number())




# ===========================================================================#
## Fit initial random forest model
# ===========================================================================#

rf1 <- ranger(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV, # specify the predictor variables and response variable
  data = df, importance = "impurity", # specify the  importance measure
  num.trees = 100, keep.inbag = TRUE #  keep the in-bag observations
)


# Generate out-of-bag residuals
residuals <- df$RATIO - rf1$predictions

# sort residuals
residuals <- data.frame(sort(residuals))
colnames(residuals)[1] <- "residuals"

residuals <- residuals %>%
  mutate(unique_id = row_number())


# get  the 25th percentile of the response values
p25_response <- quantile(df$RATIO, probs = 0.25)

# get  the 75th percentile of the response values
p75_response <- quantile(df$RATIO, probs = 0.75)


# Add extreme_id column
df <- df %>%
  mutate(extreme_id = case_when(
    RATIO <= p25_response ~ "lower_extreme",
    RATIO >= p75_response ~ "higher_extreme",
    TRUE ~ "mid_range"
  ))



# make ratio prediction
df$rf_pred <- predict(rf1, df)$predictions



# ===========================================================================#
## Partition Ratio based on extreme id
# ===========================================================================#

# lower extremes y
full_data_pred_lower <- df %>%
  filter(extreme_id == "lower_extreme")

# lower extremes y
full_data_pred_midrange <- df %>%
  filter(extreme_id == "mid_range")


# higher extremes y
full_data_pred_higher <- df %>%
  filter(extreme_id == "higher_extreme")




# ===========================================================================#
## Partition RESIDUALS based on extreme id
# ===========================================================================#

# lower extremes
lower_extreme_residuals <- residuals %>%
  filter(unique_id %in% full_data_pred_lower$unique_id)

hist(lower_extreme_residuals$residuals)

# mid range  extremes
midrange_extreme_residuals <- residuals %>%
  filter(unique_id %in% full_data_pred_midrange$unique_id)

hist(midrange_extreme_residuals$residuals)

# higher extremes
higher_extreme_residuals <- residuals %>%
  filter(unique_id %in% full_data_pred_higher$unique_id)

hist(higher_extreme_residuals$residuals)



# ===========================================================================#
## Residual bootstrap
# ===========================================================================#
# number of bootstrap iteration
n_iter <- 1000

# holder for the bootstrap trees
bootstrap_trees <- list()

for (i in 1:n_iter) {
  #######################################
  ## lower extremes residual bootstrapping
  ######################################
  # Perform residual bootstrapping
  bootstrap_samples <- matrix(sample(lower_extreme_residuals$residuals,
    replace = TRUE,
    size = length(lower_extreme_residuals$residuals)
  ), ncol = 1)

  lower_y <- full_data_pred_lower
  lower_y$inf_residual <- as.vector(bootstrap_samples)


  lower_y <- lower_y %>%
    mutate(y_bootstrap = inf_residual + rf_pred)



  #######################################
  ## mid range extremes residual bootstrapping
  ######################################
  # Perform residual bootstrapping
  bootstrap_samples <- matrix(sample(midrange_extreme_residuals$residuals,
    replace = TRUE,
    size = length(midrange_extreme_residuals$residuals)
  ), ncol = 1)

  mid_y <- full_data_pred_midrange
  mid_y$inf_residual <- as.vector(bootstrap_samples)


  mid_y <- mid_y %>%
    mutate(y_bootstrap = inf_residual + rf_pred)


  #######################################
  ## higher extremes residual bootstrapping
  ######################################
  # Perform residual bootstrapping
  bootstrap_samples <- matrix(sample(higher_extreme_residuals$residuals,
    replace = TRUE,
    size = length(higher_extreme_residuals$residuals)
  ), ncol = 1)

  higher_y <- full_data_pred_higher
  higher_y$inf_residual <- as.vector(bootstrap_samples)


  higher_y <- higher_y %>%
    mutate(y_bootstrap = inf_residual + rf_pred)



  # combine dataframes into one
  full <- rbind(higher_y, mid_y, lower_y)

  # Fit a decision tree model
  rf_temp <- ranger(
    y_bootstrap ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT +
      MWMT + TD + ELEV,
    data = full,
    importance = "impurity", num.trees = 1
  )

  bootstrap_trees[[i]] <- rf_temp
}



# ===========================================================================#
## make predictions
# ===========================================================================#


# Create the random forest object
rf_object <- combine_dt_to_rf_object(bootstrap_trees)



# Make predictions using the random forest
re_pred <- make_predictions(rf_object, df)





Metrics::mse(df$RATIO, df$rf_pred)
Metrics::mse(df$RATIO, 2 * df$rf_pred - re_pred)

(Metrics::mse(df$RATIO, 2 * df$rf_pred - re_pred) -
  Metrics::mse(df$RATIO, df$rf_pred)) / Metrics::mse(df$RATIO, df$rf_pred)

par(mfrow = c(1, 2))
plot(df$RATIO, df$rf_pred, xlim = c(0, 1), ylim = c(0, 1), 
     main = "Original ratio vs predicted ratio")
abline(a = 0, b = 1, col = "blue")

plot(df$RATIO, 2 * df$rf_pred - re_pred, xlim = c(0, 1), ylim = c(0, 1),
     main = "Original ratio vs distributed inflated residual bootstrap RF ratio")
abline(a = 0, b = 1, col = "blue")



df <- df %>%
  mutate(
    bias_corrected_SWE = (2 * rf_pred - re_pred) * maxv_SNWD,
    bias_uncorrected_SWE = rf_pred * maxv_SNWD
  )




# ===========================================================================#
## compare distr parameters
# ===========================================================================#



# ===========================================================================#
# filter stations
# ===========================================================================#
# get stations with annual max more than 30
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



ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(-2, 2))+
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load(using a distributed inflated residual bootstrapping)")

