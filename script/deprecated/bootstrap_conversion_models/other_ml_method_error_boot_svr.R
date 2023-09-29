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
library(future.apply)

library(caret)
library(doParallel)
library(e1071)
library(kernlab)




# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/make_predictions.R")
source(file = "R/combine_dt_to_rf_object.R")
source(file = "R/bootstrap_error_lnorm_fitting.R")
source(file = "R/gbm_swe_sd_percentile.R")
source(file = "R/svr_lnorm_test.R")



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



#create train data

df_train <- df %>%
  filter(data == "Train")



#create test data

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
## STEP 3: Support Vector Machines (SVM)
############################################################################





fit_svr <- svm(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV,
  data = df_train, kernel = "radial",
  cost = 1, gamma = 0.1, epsilon = 0.1
)




# ===========================================================================#
## prediction and find best percentile
# ===========================================================================#

# predict values
df_train$pred_svr <- predict(fit_svr,  df_train)

# compute error
df_train <- df_train %>%
  dplyr::mutate(error_svr = RATIO - pred_svr)


plot(df_train$RATIO, df_train$pred_svr, xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1, col = "blue")


descdist(df_train$error_svr, discrete = FALSE, boot = 1000)

fit_norm <- fitdist(df_train$error_svr, "norm")
plot(fit_norm)




# ===========================================================================#
# test data
# ===========================================================================#

# # predict values
# df_test$pred_svr <- predict(fit_svr, df_test)
# 
# df_test = df_test %>%
#   mutate(swe_svr = pred_svr*maxv_SNWD)
# 
# 
# 
# df_test_filter <- df_test %>%
#   dplyr::filter(ID %in% id$ID)
# 
# # Convert dataframe of data into list by ID
# ls_df_test_lorm <- df_test_filter %>%
#   dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
#   dplyr::group_split(ID) %>%
#   stats::setNames(., unique(df_test_filter$ID))
# 
# 
# lnorm_test_converted_svr = do.call(rbind, lapply(X = ls_df_test_lorm,
#                                                  FUN = fit_lnorm, column = "swe_svr" ))
# 
# 
# 
# save(lnorm_test_converted_svr, file = "data-raw/RObject/lnorm_test_converted_svr.RData")
# 

# ===========================================================================#
# boot error, add to prediction and get LNORM parameters
# ===========================================================================#



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


tictoc::tic()

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = 14)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)



best_percentile <- mean(unlist(
  future_lapply(ls_df_train,
                FUN = gbm_swe_sd_percentile,
                prediction = "pred_svr",
                mean = fit_norm[["estimate"]][["mean"]],
                sd = fit_norm[["estimate"]][["sd"]],
                nboot = 200,
                future.scheduling = 2, future.seed = TRUE,
                future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "gbm")
  )
))


tictoc::toc()


## Shut down parallel workers
future::plan("sequential")






# ===========================================================================#
## test data
# ===========================================================================#


tictoc::tic()

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = 14)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)

# specify percentiles
percentile <- c(0.30, 0.85, 0.9, 0.92, 0.94, 0.96, 0.98, 1)


for (i in percentile) {
  df <- do.call(
    rbind,
    future_lapply(ls_df_test,
                  FUN = svr_lnorm_test,
                  svr_model = fit_svr,
                  probs = i,
                  mean = fit_norm[["estimate"]][["mean"]],
                  sd = fit_norm[["estimate"]][["sd"]],
                  nboot = 200,
                  future.scheduling = 2, future.seed = TRUE,
                  future.packages = c(
                    "fitdistrplus", "tidyverse",
                    "matrixStats", "e1071"
                  )
    )
  )
  
  # remove decimal
  i <- i * 100
  
  # assign name to results
  assign(paste0("converted_load_para_q_", i, "_lnorm_svr"), df)
  
  file_name <- paste0("converted_load_para_q_", i, "_lnorm_svr")
  
  # save downloaded data
  save(
    list = paste0("converted_load_para_q_", i, "_lnorm_svr"),
    file = paste0("data-raw/RObject/", file_name, ".RData")
  )
  
  # remove data
  remove(list = paste0("converted_load_para_q_", i, "_lnorm_svr"))
  rm(df)
}



tictoc::toc()


## Shut down parallel workers
future::plan("sequential")




# load data
load("data-raw/RObject/converted_load_para_q_30_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_85_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_90_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_92_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_94_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_96_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_98_lnorm_svr.RData")
load("data-raw/RObject/converted_load_para_q_100_lnorm_svr.RData")
load("data-raw/RObject/direct_load_para_lnorm.RData")



# append station id
ID <- rownames(converted_load_para_q_30_lnorm_svr)
converted_load_para_q_30_lnorm_svr <- cbind(ID, converted_load_para_q_30_lnorm_svr)

ID <- rownames(converted_load_para_q_85_lnorm_svr)
converted_load_para_q_85_lnorm_svr <- cbind(ID, converted_load_para_q_85_lnorm_svr)

ID <- rownames(converted_load_para_q_90_lnorm_svr)
converted_load_para_q_90_lnorm_svr <- cbind(ID, converted_load_para_q_90_lnorm_svr)

ID <- rownames(converted_load_para_q_92_lnorm_svr)
converted_load_para_q_92_lnorm_svr <- cbind(ID, converted_load_para_q_92_lnorm_svr)

ID <- rownames(converted_load_para_q_94_lnorm_svr)
converted_load_para_q_94_lnorm_svr <- cbind(ID, converted_load_para_q_94_lnorm_svr)

ID <- rownames(converted_load_para_q_96_lnorm_svr)
converted_load_para_q_96_lnorm_svr <- cbind(ID, converted_load_para_q_96_lnorm_svr)

ID <- rownames(converted_load_para_q_98_lnorm_svr)
converted_load_para_q_98_lnorm_svr <- cbind(ID, converted_load_para_q_98_lnorm_svr)

ID <- rownames(converted_load_para_q_100_lnorm_svr)
converted_load_para_q_100_lnorm_svr <- cbind(ID, converted_load_para_q_100_lnorm_svr)






# change class to dataframe

converted_load_para_q_30_lnorm_svr <- as.data.frame(converted_load_para_q_30_lnorm_svr) %>%
  mutate(
    loc_30_const = exp(as.numeric(meanlog)),
    sd_30_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_30_const, sd_30_const)

converted_load_para_q_85_lnorm_svr <- as.data.frame(converted_load_para_q_85_lnorm_svr) %>%
  mutate(
    loc_85_const = exp(as.numeric(meanlog)),
    sd_85_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_85_const, sd_85_const)

converted_load_para_q_90_lnorm_svr <- as.data.frame(converted_load_para_q_90_lnorm_svr) %>%
  mutate(
    loc_90_const = exp(as.numeric(meanlog)),
    sd_90_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_90_const, sd_90_const)

converted_load_para_q_92_lnorm_svr <- as.data.frame(converted_load_para_q_92_lnorm_svr) %>%
  mutate(
    loc_92_const = exp(as.numeric(meanlog)),
    sd_92_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_92_const, sd_92_const)

converted_load_para_q_94_lnorm_svr <- as.data.frame(converted_load_para_q_94_lnorm_svr) %>%
  mutate(
    loc_94_const = exp(as.numeric(meanlog)),
    sd_94_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_94_const, sd_94_const)

converted_load_para_q_96_lnorm_svr <- as.data.frame(converted_load_para_q_96_lnorm_svr) %>%
  mutate(
    loc_96_const = exp(as.numeric(meanlog)),
    sd_96_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_96_const, sd_96_const)

converted_load_para_q_98_lnorm_svr <- as.data.frame(converted_load_para_q_98_lnorm_svr) %>%
  mutate(
    loc_98_const = exp(as.numeric(meanlog)),
    sd_98_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_98_const, sd_98_const)

converted_load_para_q_100_lnorm_svr <- as.data.frame(converted_load_para_q_100_lnorm_svr) %>%
  mutate(
    loc_100_const = exp(as.numeric(meanlog)),
    sd_100_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_100_const, sd_100_const)



direct_load_para_lnorm <- direct_load_para_lnorm %>%
  mutate(
    loc = exp(as.numeric(meanlog)),
    sd = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc, sd)





# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_bean_lnorm <- inner_join(
  x = direct_load_para_lnorm, y = converted_load_para_q_100_lnorm_svr,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_98_lnorm_svr,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_96_lnorm_svr,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_94_lnorm_svr,
  by = c("ID")
)


comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_92_lnorm_svr,
  by = c("ID")
)


comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_90_lnorm_svr,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_85_lnorm_svr,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_30_lnorm_svr,
  by = c("ID")
)




# create relative ratios
comb_para_bean_lnorm <- comb_para_bean_lnorm %>%
  mutate(
    ratio_loc_100_const = loc_100_const / loc,
    ratio_sd_100_const = sd_100_const / sd,
    ratio_loc_98_const = loc_98_const / loc,
    ratio_sd_98_const = sd_98_const / sd,
    ratio_loc_96_const = loc_96_const / loc,
    ratio_sd_96_const = sd_96_const / sd,
    ratio_loc_94_const = loc_94_const / loc,
    ratio_sd_94_const = sd_94_const / sd,
    ratio_loc_92_const = loc_92_const / loc,
    ratio_sd_92_const = sd_92_const / sd,
    ratio_loc_90_const = loc_90_const / loc,
    ratio_sd_90_const = sd_90_const / sd,
    ratio_loc_85_const = loc_85_const / loc,
    ratio_sd_85_const = sd_85_const / sd,
    ratio_loc_30_const = loc_30_const / loc,
    ratio_sd_30_const = sd_30_const / sd
  )




data_long <- comb_para_bean_lnorm %>% pivot_longer(
  cols = c(
    "ratio_loc_100_const" , "ratio_sd_100_const" , "ratio_loc_98_const" ,
    "ratio_sd_98_const" ,"ratio_loc_96_const" , "ratio_sd_96_const" ,
    "ratio_loc_94_const" , "ratio_sd_94_const" ,"ratio_loc_92_const" ,
    "ratio_sd_92_const" ,"ratio_loc_90_const" , "ratio_sd_90_const" ,
    "ratio_loc_85_const" , "ratio_sd_85_const" ,"ratio_loc_30_const" ,
    "ratio_sd_30_const" 
  ),
  names_to = "variable", values_to = "value"
)


data_long <- data_long %>%
  mutate(method = case_when(
    variable == "ratio_loc_100_const" ~ "100p of parameters",
    variable == "ratio_sd_100_const" ~ "100p of parameters",
    variable == "ratio_loc_98_const" ~ "98p of parameters",
    variable == "ratio_sd_98_const" ~ "98p of parameters",
    variable == "ratio_loc_96_const" ~ "96p of parameters",
    variable == "ratio_sd_96_const" ~ "96p of parameters",
    variable == "ratio_loc_94_const" ~ "94p of parameters",
    variable == "ratio_sd_94_const" ~ "94p of parameters",
    variable == "ratio_loc_92_const" ~ "92p of parameters",
    variable == "ratio_sd_92_const" ~ "92p of parameters",
    variable == "ratio_loc_90_const" ~ "90p of parameters",
    variable == "ratio_sd_90_const" ~ "90p of parameters",
    variable == "ratio_loc_85_const" ~ "85p of parameters",
    variable == "ratio_sd_85_const" ~ "85p of parameters",
    variable == "ratio_loc_30_const" ~ "30p of parameters",
    variable == "ratio_sd_30_const" ~ "30p of parameters"
  ))



data_long <- data_long %>%
  mutate(variable = case_when(
    variable == "ratio_loc_100_const" ~ "mean",
    variable == "ratio_loc_98_const" ~ "mean",
    variable == "ratio_loc_96_const" ~ "mean",
    variable == "ratio_loc_94_const" ~ "mean",
    variable == "ratio_loc_92_const" ~ "mean",
    variable == "ratio_loc_90_const" ~ "mean",
    variable == "ratio_loc_85_const" ~ "mean",
    variable == "ratio_loc_30_const" ~ "mean",
    variable == "ratio_sd_100_const" ~ "sd",
    variable == "ratio_sd_98_const" ~ "sd",
    variable == "ratio_sd_96_const" ~ "sd",
    variable == "ratio_sd_94_const" ~ "sd",
    variable == "ratio_sd_92_const" ~ "sd",
    variable == "ratio_sd_90_const" ~ "sd",
    variable == "ratio_sd_85_const" ~ "sd",
    variable == "ratio_sd_30_const" ~ "sd"
  ))


data_long$method = as.factor(data_long$method)
levels(data_long$method)

#re-order factor levels 
data_long$method <- factor(data_long$method, 
                           levels = c("100p of parameters", "98p of parameters" , 
                                      "96p of parameters" , "94p of parameters",
                                      "92p of parameters" , "90p of parameters",
                                      "30p of parameters" , "85p of parameters" ))

# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2)  +
  ylim(c(0.6, 1.4)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load with different paramter percentile
          (SVR: percentile 30p) ")


























