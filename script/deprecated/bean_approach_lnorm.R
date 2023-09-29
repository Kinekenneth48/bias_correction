

##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries and functions
# =============================================================================#
library(ranger)
library(renv)
library(fitdistrplus)
library(tidyverse)
library(matrixStats)
library(future)
library(future.apply)

# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/fit_distribution.R")
source(file = "R/tree_lnorm_fitting.R")

# ===========================================================================#
# load data and preprocess
# ===========================================================================#
df <- read.csv("data-raw/data.csv") # Read the data from data.csv file


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

# make ratio prediction
df$full_pred <- predict(rf1, df)$predictions

#compute SWE from ratio prediction
df = df %>%
  mutate(swe_rf = full_pred * maxv_SNWD)






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




##############################################################################
# STEP 1: apply Bean's approach of accounting for variance from RF into
#  distribution fitting
##############################################################################

# ===========================================================================#
# Fit different decision trees and get LNORM parameters
# ===========================================================================#


tictoc::tic()

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = 14)


converted_load_para_mean_lnorm <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = tree_lnorm_fitting,
    n_trees = 500,
    mean_para = TRUE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)



converted_load_para_q_75_lnorm <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = tree_lnorm_fitting,
    n_trees = 500,
    probs = 0.75,
    mean_para = FALSE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = TRUE,
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)



converted_load_para_q_90_lnorm <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = tree_lnorm_fitting,
    n_trees = 500,
    probs = 0.90,
    mean_para = FALSE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = TRUE,
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)

tictoc::toc()


## Shut down parallel workers
future::plan("sequential")



# append station id
ID <- rownames(converted_load_para_mean_lnorm)
converted_load_para_mean_lnorm <- cbind(ID, converted_load_para_mean_lnorm)


# append station id
ID <- rownames(converted_load_para_q_75_lnorm)
converted_load_para_q_75_lnorm <- cbind(ID, converted_load_para_q_75_lnorm)

# append station id
ID <- rownames(converted_load_para_q_90_lnorm)
converted_load_para_q_90_lnorm <- cbind(ID, converted_load_para_q_90_lnorm)

# save output
save(converted_load_para_mean_lnorm, file = "data-raw/RObject/converted_load_para_mean_lnorm.RData")
save(converted_load_para_q_75_lnorm, file = "data-raw/RObject/converted_load_para_q_75_lnorm.RData")
save(converted_load_para_q_90_lnorm, file = "data-raw/RObject/converted_load_para_q_90_lnorm.RData")





# ===========================================================================#
# get LNORM parameters for direct loads
# ===========================================================================#

direct_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "maxv_WESD"
)) %>% mutate(mean = exp(meanlog), sd = exp(sdlog))



# save output
save(direct_load_para_lnorm, file = "data-raw/RObject/direct_load_para_lnorm.RData")




# ===========================================================================#
# get LNORM parameters for RF predicted loads
# ===========================================================================#

rf_load_para_lnorm <- do.call(rbind, lapply(ls_df,
                                                FUN = fit_lnorm,
                                                "swe_rf"
))



# save output
save(rf_load_para_lnorm, file = "data-raw/RObject/rf_load_para_lnorm.RData")





##############################################################################
# STEP 2: Compute relative parameter ratios for converted loads
##############################################################################


# ===========================================================================#
# load data and preprocessing
# ===========================================================================#

# load converted and direct load parameter data
load("data-raw/RObject/direct_load_para_lnorm.RData")
load("data-raw/RObject/rf_load_para_lnorm.RData")
load("data-raw/RObject/converted_load_para_mean_lnorm.RData")
load("data-raw/RObject/converted_load_para_q_75_lnorm.RData")
load("data-raw/RObject/converted_load_para_q_90_lnorm.RData")


# change class to dataframe
converted_load_para_mean_lnorm <- as.data.frame(converted_load_para_mean_lnorm) %>%
  dplyr::mutate(
    loc_mean = exp(as.numeric(meanlog)),
    sd_mean = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_mean, sd_mean)


converted_load_para_q_75_lnorm <- as.data.frame(converted_load_para_q_75_lnorm) %>%
  mutate(
    loc_75 = exp(as.numeric(meanlog)),
    sd_75 = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_75, sd_75)



converted_load_para_q_90_lnorm <- as.data.frame(converted_load_para_q_90_lnorm) %>%
  mutate(
    loc_90 = exp(as.numeric(meanlog)),
    sd_90 = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_90, sd_90)


direct_load_para_lnorm <- direct_load_para_lnorm %>%
  mutate(
    loc = exp(as.numeric(meanlog)),
    sd = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc, sd)


rf_load_para_lnorm <- rf_load_para_lnorm %>%
  mutate(
    loc_rf = exp(as.numeric(meanlog)),
    sd_rf = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_rf, sd_rf)




# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_bean_lnorm <- inner_join(
  x = direct_load_para_lnorm, y = converted_load_para_q_90_lnorm,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_75_lnorm,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_mean_lnorm,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = rf_load_para_lnorm,
  by = c("ID")
)


# create relative ratios
comb_para_bean_lnorm <- comb_para_bean_lnorm %>%
  mutate(
    ratio_loc_mean = loc_mean / loc,
    ratio_sd_mean = sd_mean / sd,
    ratio_loc_90 = loc_90 / loc,
    ratio_sd_90 = sd_90 / sd,
    ratio_loc_75 = loc_75 / loc,
    ratio_sd_75 = sd_75 / sd,
    ratio_loc_rf = loc_rf / loc,
    ratio_sd_rf = sd_rf / sd
  )



data_long_bean <- comb_para_bean_lnorm %>% 
  pivot_longer(
  cols = c(
    "ratio_loc_mean", "ratio_sd_mean", 
    "ratio_loc_90", "ratio_sd_90", 
    "ratio_loc_75", "ratio_sd_75",
    "ratio_loc_rf", "ratio_sd_rf"
  ),
  names_to = "variable", values_to = "value"
)



data_long_bean <- data_long_bean %>%
  mutate(method = case_when(
    variable == "ratio_loc_mean" ~ "mean of parameters",
    variable == "ratio_sd_mean" ~ "mean of parameters",
    variable == "ratio_loc_90" ~ "90p of parameters",
    variable == "ratio_sd_90" ~ "90p of parameters",
    variable == "ratio_loc_75" ~ "75p of parameters",
    variable == "ratio_sd_75" ~ "75p of parameters",
    variable == "ratio_loc_rf" ~ "RF of parameters",
    variable == "ratio_sd_rf" ~ "RF of parameters"
  ))




# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#



ggplot(data_long_bean, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.6)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load(using Bean's method)")
