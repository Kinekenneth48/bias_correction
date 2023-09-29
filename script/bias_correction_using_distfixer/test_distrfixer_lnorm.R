#############################################################################
# This script is created to bias correct the log normal distr.'s spread parameter
# using the distfixer package created. The data used here is the SWE data used 
# in Jess's paper
##############################################################################

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
library(distfixer)

# Source the R file
source(file = "R/fit_lnorm.R")

set.seed(1234)


# ===========================================================================#
# load data and preprocess
# ===========================================================================#
df <- read.csv("data-raw/data.csv") # jess data

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


# ==========================================================================#
# filter train data
# ===========================================================================#

df_train <- df %>%
  filter(data == "Train")


# Filter stations with annual max more than 30
id <- df_train %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 30)


df_train_filter <- df_train %>%
  dplyr::filter(ID %in% id$ID)


# Convert dataframe of data into list by ID
ls_df_train <- df_train_filter %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df_train_filter$ID))



# ==========================================================================#
# filter test data
# ===========================================================================#
df_test <- df %>%
  filter(data == "Test")

# Filter stations with annual max more than 30
id <- df_test %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 20)

df_test_filter <- df_test %>%
  dplyr::filter(ID %in% id$ID)

# Convert dataframe of data into list by ID
ls_df_test <- df_test_filter %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df_test_filter$ID))



############################################################################
## STEP 1: direct load
############################################################################

test_direct_lnorm_para <- do.call(rbind, lapply(
  X = ls_df_test,
  FUN = fit_lnorm, column = "maxv_WESD"
))

save(test_direct_lnorm_para, file = "data-raw/RObject/test_direct_lnorm_para.RData")



############################################################################
## STEP 1: RF
############################################################################

# ==========================================================================#
# fit model
# ===========================================================================#
fit_rf <- distfixer::fit_model(
  formula = RATIO ~ logSNWD + SMONTH + D2C +
    logPPTWT + MCMT + MWMT + TD + ELEV,
  data = df_train, method = "rf"
)


save(fit_rf, file = "data-raw/RObject/fit_rf.RData")


# df_train$r = predict(fit_rf, df_train)$predictions
# 
# df_ecdf <- df_train %>%
#   dplyr::mutate(ecdf_load = r * maxv_SNWD)
# 
# # Convert dataframe of data into list by ID
# df_ecdf <- df_ecdf %>%
#   dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
#   dplyr::group_split(ID) %>%
#   stats::setNames(., unique(df_ecdf$ID))
# 
# ecdf_eg = df_ecdf[["10E15:WY:SNOW"]]
# 
# ecdf_eg = ecdf_eg %>%
#   select(ID, NAME, STATE, maxv_WESD, ecdf_load)
# 
# save(ecdf_eg, file = "data-raw/RObject/ecdf_eg.RData")


# ==========================================================================#
# make predictions on test data before bias correction
# ===========================================================================#

test_unadjusted <- do.call(rbind, ls_df_test)

test_unadjusted$pred_ratio <- predict(fit_rf, test_unadjusted)$predictions

test_unadjusted <- test_unadjusted %>%
  dplyr::mutate(pred_load = pred_ratio * maxv_SNWD)



# Convert dataframe of data into list by ID
ls_test_unadjusted <- test_unadjusted %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(test_unadjusted$ID))

# ==========================================================================#
# fit lnorm on test data without bais correction
# ===========================================================================#
lnorm_test_unadjusted_rf <- do.call(
  rbind,
  lapply(
    X = ls_test_unadjusted, FUN = fit_lnorm,
    column = "pred_load"
  )
)

save(lnorm_test_unadjusted_rf, file = "data-raw/RObject/lnorm_test_unadjusted_rf.RData")



# ==========================================================================#
# get error distr.
# ===========================================================================#
# get error distr.
error_rf <- distfixer::get_error_distribution(
  train_data = df_train,
  fitted_model = fit_rf, label = "RATIO"
)

mean_rf <- error_rf[["estimate"]][["mean"]]
sd_rf <- error_rf[["estimate"]][["sd"]]


# ==========================================================================#
# find percentile
# ===========================================================================#


## Run the analysis in parallel on the local computer
future::plan("multisession", workers = parallel::detectCores() - 2)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# get the percentiles
train_rf_per <- future_lapply(
  X = ls_df_train, FUN = distfixer::best_percentile,
  direct_label = "RATIO", bounds = c(0.0001, Inf),
  fitted_model = fit_rf, mean = mean_rf, sd = sd_rf,
  nboot = 500, distr = "lnorm",
  param_adjust = "sdlog", label_convert = TRUE,
  multiplier = "maxv_SNWD",
  indirect_label = "maxv_WESD",
  future.scheduling = 2, future.seed = TRUE,
  future.packages = c("tidyverse", "distfixer")
)

## Shut down parallel workers
future::plan("sequential")


# find the mean of percentile
mean_percentile_rf <- mean(unlist(train_rf_per))
mean_percentile_rf



# ==========================================================================#
# predict on test data
# ===========================================================================


## Run the analysis in parallel on the local computer
future::plan("multisession", workers = parallel::detectCores() - 2)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# get the percentiles
test_rf_lnorm_para <- do.call(rbind, future_lapply(
  X = ls_df_test, FUN = distfixer::predict_param,
  direct_label = "RATIO", bounds = c(0.0001, Inf),
  fitted_model = fit_rf, mean = mean_rf, sd = sd_rf,
  nboot = 500, distr = "lnorm", all_missing = TRUE,
  param_adjust = "sdlog", label_convert = TRUE,
  multiplier = "maxv_SNWD", percentile = mean_percentile_rf,
  indirect_label = "maxv_WESD",
  future.scheduling = 2, future.seed = TRUE,
  future.packages = c("tidyverse", "distfixer")
))
## Shut down parallel workers
future::plan("sequential")

save(test_rf_lnorm_para, file = "data-raw/RObject/test_rf_lnorm_para.RData")







############################################################################
## STEP 2: gbm
############################################################################


# ==========================================================================#
# fit model
# ===========================================================================#
# fit model
fit_gbm <- distfixer::fit_model(
  formula = RATIO ~ logSNWD + SMONTH + D2C +
    logPPTWT + MCMT + MWMT + TD + ELEV,
  data = df_train, method = "gbm"
)


save(fit_gbm, file = "data-raw/RObject/fit_gbm.RData")


# ==========================================================================#
# make predictions on test data before bias correction
# ===========================================================================#

test_unadjusted = do.call(rbind, ls_df_test)

test_unadjusted$pred_ratio = predict(fit_gbm , test_unadjusted)

test_unadjusted = test_unadjusted %>%
  dplyr::mutate(pred_load = pred_ratio*maxv_SNWD)



# Convert dataframe of data into list by ID
ls_test_unadjusted  <- test_unadjusted %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(test_unadjusted$ID))

# ==========================================================================#
# fit gev on test data without bais correction
# ===========================================================================#
lnorm_test_unadjusted_gbm = do.call(rbind, 
                                  lapply(X = ls_test_unadjusted , FUN = fit_lnorm,
                                         column = "pred_load" ))

save(lnorm_test_unadjusted_gbm, file = "data-raw/RObject/lnorm_test_unadjusted_gbm.RData")


# ==========================================================================#
# get error distr.
# ===========================================================================#

error_gbm <- distfixer::get_error_distribution(
  train_data = df_train,
  fitted_model = fit_gbm, label = "RATIO"
)


mean_gbm <- error_gbm[["estimate"]][["mean"]]
sd_gbm <- error_gbm[["estimate"]][["sd"]]


# ==========================================================================#
# find percentile
# ===========================================================================#


## Run the analysis in parallel on the local computer
future::plan("multisession", workers = parallel::detectCores() - 2)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# get the percentiles
train_gbm_per <- future_lapply(
  X = ls_df_train, FUN = distfixer::best_percentile,
  direct_label = "RATIO", bounds = c(0.0001, Inf),
  fitted_model = fit_gbm, mean = mean_gbm, sd = sd_gbm,
  nboot = 500, distr = "lnorm",
  param_adjust = "sdlog", label_convert = TRUE,
  multiplier = "maxv_SNWD",
  indirect_label = "maxv_WESD",
  future.scheduling = 2, future.seed = TRUE,
  future.packages = c("tidyverse", "distfixer")
)

## Shut down parallel workers
future::plan("sequential")


# find the mean of percentile
mean_percentile_gbm <- mean(unlist(train_gbm_per))
mean_percentile_gbm



# ==========================================================================#
# predict on test data
# ===========================================================================


## Run the analysis in parallel on the local computer
future::plan("multisession", workers = parallel::detectCores() - 2)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# get the percentiles
test_gbm_lnorm_para <- do.call(rbind, future_lapply(
  X = ls_df_test, FUN = distfixer::predict_param,
  direct_label = "RATIO", bounds = c(0.0001, Inf),
  fitted_model = fit_gbm, mean = mean_gbm, sd = sd_gbm,
  nboot = 500, distr = "lnorm", all_missing = TRUE,
  param_adjust = "sdlog", label_convert = TRUE,
  multiplier = "maxv_SNWD", percentile = mean_percentile_gbm,
  indirect_label = "maxv_WESD",
  future.scheduling = 2, future.seed = TRUE,
  future.packages = c("tidyverse", "distfixer")
))
## Shut down parallel workers
future::plan("sequential")

save(test_gbm_lnorm_para, file = "data-raw/RObject/test_gbm_lnorm_para.RData")







############################################################################
## STEP 3: Support Vector Machines (SVM)
############################################################################


# ==========================================================================#
# fit model
# ===========================================================================#
# fit model
fit_svr <- distfixer::fit_model(
  formula = RATIO ~ logSNWD + SMONTH + D2C +
    logPPTWT + MCMT + MWMT + TD + ELEV,
  data = df_train, method = "svr"
)


save(fit_svr, file = "data-raw/RObject/fit_svr.RData")



# ==========================================================================#
# make predictions on test data
# ===========================================================================#

test_unadjusted = do.call(rbind, ls_df_test)

test_unadjusted$pred_ratio = kernlab::predict(fit_svr , test_unadjusted)

test_unadjusted = test_unadjusted %>%
  dplyr::mutate(pred_load = pred_ratio*maxv_SNWD)



# Convert dataframe of data into list by ID
ls_test_unadjusted  <- test_unadjusted %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(test_unadjusted$ID))

# ==========================================================================#
# fit lnorm on test data without bais correction
# ===========================================================================#
lnorm_test_unadjusted_svr = do.call(rbind, 
        lapply(X = ls_test_unadjusted , FUN = fit_lnorm,  column = "pred_load" ))

save(lnorm_test_unadjusted_svr, file = "data-raw/RObject/lnorm_test_unadjusted_svr.RData")



# ==========================================================================#
# get error distr.
# ===========================================================================#

error_svr <- distfixer::get_error_distribution(
  train_data = df_train,
  fitted_model = fit_svr, label = "RATIO"
)


mean_svr <- error_svr[["estimate"]][["mean"]]
sd_svr <- error_svr[["estimate"]][["sd"]]


# ==========================================================================#
# find percentile
# ===========================================================================#


## Run the analysis in parallel on the local computer
future::plan("multisession", workers = parallel::detectCores() - 2)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# get the percentiles
train_svr_per <- future_lapply(
  X = ls_df_train, FUN = distfixer::best_percentile,
  direct_label = "RATIO", bounds = c(0.0001, Inf),
  fitted_model = fit_svr, mean = mean_svr, sd = sd_svr,
  nboot = 500, distr = "lnorm",
  param_adjust = "sdlog", label_convert = TRUE,
  multiplier = "maxv_SNWD",
  indirect_label = "maxv_WESD",
  future.scheduling = 2, future.seed = TRUE,
  future.packages = c("tidyverse", "distfixer")
)

## Shut down parallel workers
future::plan("sequential")


# find the mean of percentile
mean_percentile_svr <- mean(unlist(train_svr_per))
mean_percentile_svr



# ==========================================================================#
# predict on test data
# ===========================================================================

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = parallel::detectCores() - 2)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# get the percentiles
test_svr_lnorm_para <- do.call(rbind, future_lapply(
  X = ls_df_test, FUN = distfixer::predict_param,
  direct_label = "RATIO", bounds = c(0.0001, Inf),
  fitted_model = fit_svr, mean = mean_svr, sd = sd_svr,
  nboot = 500, distr = "lnorm", all_missing = TRUE,
  param_adjust = "sdlog", label_convert = TRUE,
  multiplier = "maxv_SNWD", percentile = mean_percentile_svr,
  indirect_label = "maxv_WESD",
  future.scheduling = 2, future.seed = TRUE,
  future.packages = c("tidyverse", "distfixer")
))
## Shut down parallel workers
future::plan("sequential")

save(test_svr_lnorm_para, file = "data-raw/RObject/test_svr_lnorm_para.RData")
