############################################################################
## STEP 0: INITIAL STEP
############################################################################

# ===========================================================================#
# load libraries and functions
# ===========================================================================#
library(ranger)
library(Metrics)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(fitdistrplus)
library(gbm) # basic implementation
library(xgboost) # a faster implementation of gbm
library(e1071)
library(tensorflow)
library(keras)
library(tidymodels)
library(recipes)


use_virtualenv("r-reticulate")
tensorflow::tf_config()
reticulate::py_config()

# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/make_predictions.R")
source(file = "R/combine_dt_to_rf_object.R")

# for reproducibility
set.seed(12358)

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




############################################################################
## STEP 1: NEURAL NETWORK
############################################################################


df_nn <- df %>%
  dplyr::select(logSNWD, SMONTH, D2C, logPPTWT, MCMT, MWMT, TD, ELEV, RATIO)

# ===========================================================================#
# split features from labels
# ===========================================================================#

df_features <- df_nn %>% dplyr::select(-RATIO)
df_labels <- df_nn %>% dplyr::select(RATIO)


# ===========================================================================#
# create normalizer
# ===========================================================================#

normalizer <- keras::layer_normalization(axis = -1L)

normalizer %>% keras::adapt(as.matrix(df_features))

print(normalizer$mean)



# ===========================================================================#
# BUILD NN - model definition
# ===========================================================================#

build_and_compile <- function(norm) {
  model <- keras::keras_model_sequential() %>%
    norm() %>%
    keras::layer_dense(units = 25, activation = "relu") %>%
    keras::layer_dense(1)

  model %>% keras::compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = 0.001)
  )

  return(model)
}



# ===========================================================================#
# Compile NN model
# ===========================================================================#

dnn_model <- build_and_compile(normalizer)
summary(dnn_model)




# ===========================================================================#
# Fit NN model
# ===========================================================================#


history <- dnn_model %>%
  fit(
    as.matrix(df_features),
    as.matrix(df_labels),
    validation_split = 0.2,
    verbose = 0,
    epochs = 50
  )

plot(history)

test_results <- list()

test_results[["dnn_model"]] <- dnn_model %>%
  evaluate(
    as.matrix(df_features),
    as.matrix(df_labels),
    verbose = 0
  )

sapply(test_results, function(x) x)


# ===========================================================================#
#  NN predictions
# ===========================================================================#

df$pred_nn <- predict(dnn_model, as.matrix(df_features))











############################################################################
## STEP 2: GBM
############################################################################



# ===========================================================================#
## Fit initial GBM model
# ===========================================================================#


# # for reproducibility
# set.seed(123)
#
# # train GBM model
# gbm.fit <- gbm::gbm(
#   formula = RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
#     TD + ELEV,
#   distribution = "gaussian",
#   data = df,
#   n.trees = 10000,
#   interaction.depth = 1,
#   shrinkage = 0.001,
#   cv.folds = 5,
#   n.cores = 8,
#   verbose = FALSE
# )
#
# # find index for n trees with minimum CV error
# min_MSE <- which.min(gbm.fit$cv.error)
#
# # print results
# print(gbm.fit)
#
#
# # get MSE and compute RMSE
# sqrt(gbm.fit$cv.error[min_MSE])
#
#
# # plot loss function as a result of n trees added to the ensemble
# gbm::gbm.perf(gbm.fit, method = "cv")
#
#
# # ===========================================================================#
# ## manually tweaking hyperparameters
# # ===========================================================================#
# library(foreach)
# library(doParallel)
#
#
# # Register a parallel backend
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)
#
#
#
# # create hyperparameter grid
# hyper_grid <- expand.grid(
#   shrinkage = c(.01, .1, .3),
#   interaction.depth = c(1, 3, 5),
#   n.minobsinnode = c(5, 10, 15),
#   bag.fraction = c(.65, .8, 1),
#   optimal_trees = 0, # a place to dump results
#   min_RMSE = 0 # a place to dump results
# )
#
# # total number of combinations
# nrow(hyper_grid)
#
# # Define the tuning function
# tuning_fun <- function(hyper_grid) {
#
#
#     gbm.tune = gbm::gbm(
#       formula = RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
#         TD + ELEV,
#       distribution = "gaussian",
#       data = random_df_train,
#       n.trees = 5000,
#       interaction.depth = hyper_grid[,"interaction.depth"][i],
#       shrinkage = hyper_grid[,"shrinkage"][i],
#       n.minobsinnode = hyper_grid[,"n.minobsinnode"][i],
#       bag.fraction = hyper_grid[,"bag.fraction"][i],
#       train.fraction = .75,
#       verbose = FALSE
#     )
#
#     # add min training error and trees to grid
#     hyper_grid[,'optimal_trees'][i] <- which.min(gbm.tune$valid.error)
#     hyper_grid[,"min_RMSE"][i] <- sqrt(min(gbm.tune$valid.error))
#
#
# }
#
#
# # randomize data
# random_index <- sample(1:nrow(df), nrow(df))
# random_df_train <- df[random_index, ]
#
# # Use "foreach" to parallelize the tuning process
# fits <- foreach(i = 1:nrow(hyper_grid)) %dopar% {
#
#   gbm.tune = gbm::gbm(
#     formula = RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
#       TD + ELEV,
#     distribution = "gaussian",
#     data = random_df_train,
#     n.trees = 5000,
#     interaction.depth = hyper_grid[,"interaction.depth"][i],
#     shrinkage = hyper_grid[,"shrinkage"][i],
#     n.minobsinnode = hyper_grid[,"n.minobsinnode"][i],
#     bag.fraction = hyper_grid[,"bag.fraction"][i],
#     train.fraction = .75,
#     verbose = FALSE
#   )
#
#    df = hyper_grid[i,]
#
#
#   # add min training error and trees to grid
#   df$optimal_trees <- which.min(gbm.tune$valid.error)
#   df$min_RMSE <- sqrt(min(gbm.tune$valid.error))
#
#
#   return(df)
#
# }
#
#
# hyper_grid_results = do.call(rbind, fits)
#
# # Stop the parallel backend
# stopCluster(cl)
#
#
#
#
#
# hyper_grid_results %>%
#   dplyr::arrange(min_RMSE) %>%
#   head(10)




# ===========================================================================#
## Fit second GBM model
# ===========================================================================#




# train GBM model
gbm.fit2 <- gbm::gbm(
  formula = RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV,
  distribution = "gaussian",
  data = df,
  n.trees = 3815,
  n.minobsinnode = 15,
  interaction.depth = 3,
  bag.fraction = 1,
  shrinkage = 0.01,
  cv.folds = 5,
  n.cores = 8,
  verbose = FALSE
)

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)

# print results
print(gbm.fit2)


# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])


# plot loss function as a result of n trees added to the ensemble
gbm::gbm.perf(gbm.fit2, method = "cv")




# ===========================================================================#
## Variable importance
# ===========================================================================#

par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit2,
  cBars = 10,
  las = 2
)

vip::vip(gbm.fit2)





# ===========================================================================#
## prediction
# ===========================================================================#

# predict values
df$pred_gbm <- predict(gbm.fit2, n.trees = gbm.fit2$n.trees, df)

# GBM ERROR
df <- df %>%
  dplyr::mutate(error_gbm = RATIO - pred_gbm)


plot(df$RATIO, df$pred_gbm, xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1, col = "blue")










############################################################################
## STEP 3: Support Vector Machines (SVM)
############################################################################

library(caret)
library(doParallel)
library(e1071)
library(kernlab)
# Set the number of cores to use
numCores <- detectCores()
registerDoParallel(numCores)


# Create the tuning grid for the support vector regression
tuningGrid <- expand.grid(
  C = seq(0.1, 10, by = 0.1),
  sigma = seq(0.1, 1, by = 0.1)
)

# Set the control parameters for the caret train function
ctrl <- trainControl(
  method = "repeatedcv", number = 10, repeats = 3,
  allowParallel = TRUE, verboseIter = FALSE
)

# Train the support vector regression model using the tuning grid and the control parameters
svmModel <- train(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV,
  data = df, method = "svmRadial",
  trControl = ctrl, tuneGrid = tuningGrid,
  tuneLength = 10,
  kernel = "radial"
)

# Print the tuned parameters and the corresponding RMSE value
svmModel



#
# x = df %>%
#   dplyr::select(logSNWD ,SMONTH , D2C , logPPTWT  , MCMT , MWMT ,TD , ELEV)
#
# y = df %>%
#   dplyr::select(RATIO)
#
# # Define a grid of parameter values to search over
# grid <- expand.grid(cost = c(0.1, 1, 10),
#                     gamma = c(0.01, 0.1, 1),
#                     epsilon = c(0.1, 0.01, 0.001))
#
#
#
# library(EZtune)
# fit.tune  = eztune(x = x ,y=y , method = "svm" , optimizer = "hjn")
#
#
# # Tune the SVM regression model using the grid of parameter values
# fit.tune <- tune(svm,  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
#                    TD + ELEV,
#                  data = df,
#                  type = "eps-regression",
#                  ranges = grid,
#                  trace = FALSE)
#
# stopCluster(cl)
# # Summarize the tuning results
# summary(fit.tune)


fit_svr <- svm(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV,
  data = df, kernel = "radial",
  cost = 1, gamma = 0.1, epsilon = 0.1
)




# ===========================================================================#
## prediction
# ===========================================================================#


# predict values
df$pred_svr <- predict(fit_svr, data = df)







############################################################################
## STEP 4:  RF
############################################################################

rf1 <- ranger(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV, # Define the predictor variables and target variable
  data = df, importance = "impurity",
  num.trees = 100, keep.inbag = TRUE # Fit a random forest model with 100 trees and save in-bag observations
)

df$pred_rf <- predict(rf1, df)$predictions

############################################################################
## STEP 4: Compute SWE from predict ratios
############################################################################

df$pred_nn <- as.numeric(df$pred_nn)

df <- df %>%
  dplyr::mutate(
    swe_nn = pred_nn * maxv_SNWD,
    swe_svr = pred_svr * maxv_SNWD,
    swe_gbm = pred_gbm * maxv_SNWD,
    swe_rf = pred_rf * maxv_SNWD
  )




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


############################################################################
## STEP 5: Fit the lognormal distr
############################################################################


direct_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "maxv_WESD"
)) %>%
  mutate(
    mean = exp(meanlog),
    sd = exp(sdlog)
  ) %>%
  dplyr::select(ID, mean, sd)


nn_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "swe_nn"
)) %>%
  mutate(
    nn_mean = exp(meanlog),
    nn_sd = exp(sdlog)
  ) %>%
  dplyr::select(ID, nn_mean, nn_sd)


rf_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "swe_rf"
)) %>%
  mutate(
    rf_mean = exp(meanlog),
    rf_sd = exp(sdlog)
  ) %>%
  dplyr::select(ID, rf_mean, rf_sd)


gbm_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "swe_gbm"
)) %>%
  mutate(
    gbm_mean = exp(meanlog),
    gbm_sd = exp(sdlog)
  ) %>%
  dplyr::select(ID, gbm_mean, gbm_sd)


svr_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "swe_svr"
)) %>%
  mutate(
    svr_mean = exp(meanlog),
    svr_sd = exp(sdlog)
  ) %>%
  dplyr::select(ID, svr_mean, svr_sd)



# combine distr. para into one dataframe
comb_para_lnorm <- inner_join(direct_load_para_lnorm,
  nn_load_para_lnorm,
  by = c("ID")
)

comb_para_lnorm <- inner_join(comb_para_lnorm,
  gbm_load_para_lnorm,
  by = c("ID")
)

comb_para_lnorm <- inner_join(comb_para_lnorm,
  svr_load_para_lnorm,
  by = c("ID")
)

comb_para_lnorm <- inner_join(comb_para_lnorm,
  rf_load_para_lnorm,
  by = c("ID")
)


comb_para_lnorm <- comb_para_lnorm %>%
  mutate(
    ratio_mean_nn = nn_mean / mean,
    ratio_sd_nn = nn_sd / sd,
    ratio_mean_svr = svr_mean / mean,
    ratio_sd_svr = svr_sd / sd,
    ratio_mean_gbm = gbm_mean / mean,
    ratio_sd_gbm = gbm_sd / sd,
    ratio_mean_rf = rf_mean / mean,
    ratio_sd_rf = rf_sd / sd
  )





data_long <- comb_para_lnorm %>%
  pivot_longer(
    cols = c(
      "ratio_mean_nn", "ratio_sd_nn", "ratio_mean_svr",
      "ratio_sd_svr", "ratio_mean_gbm", "ratio_sd_gbm",
      "ratio_mean_rf", "ratio_sd_rf"
    ),
    names_to = "variable", values_to = "value"
  )



data_long <- data_long %>%
  mutate(method = case_when(
    variable == "ratio_mean_nn" ~ "NN",
    variable == "ratio_sd_nn" ~ "NN",
    variable == "ratio_mean_svr" ~ "SVR",
    variable == "ratio_sd_svr" ~ "SVR",
    variable == "ratio_mean_gbm" ~ "GBM",
    variable == "ratio_sd_gbm" ~ "GBM",
    variable == "ratio_mean_rf" ~ "RF",
    variable == "ratio_sd_rf" ~ "RF"
  ))


data_long <- data_long %>%
  mutate(variable = case_when(
    variable == "ratio_mean_nn" ~ "mean",
    variable == "ratio_sd_nn" ~ "sd",
    variable == "ratio_mean_gbm" ~ "mean",
    variable == "ratio_sd_gbm" ~ "sd",
    variable == "ratio_mean_svr" ~ "mean",
    variable == "ratio_sd_svr" ~ "sd",
    variable == "ratio_mean_rf" ~ "mean",
    variable == "ratio_sd_rf" ~ "sd"
  ))


# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.6)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load(ML method)")
