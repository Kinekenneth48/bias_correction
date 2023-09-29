

##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries and functions
# =============================================================================#
library(ranger)
library(fitdistrplus)
library(tidyverse)
library(matrixStats)
library(future)
library(future.apply)

# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/fit_distribution.R")
source(file = "R/tree_lnorm_fitting.R")
source(file = "R/rf_swe_sd_percentile.R")
source(file = "R/rf_prediction_lnorm_test.R")
source(file = "R/tree_lnorm_test.R")
source(file = "R/constrained_tree_fit.R")



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


# ===========================================================================#
## test data
# ===========================================================================#

rf1 <- ranger(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV, # Define the predictor variables and target variable
  data = df_train, importance = "impurity",
  num.trees = 100, keep.inbag = TRUE # Fit a random forest model with 100 trees and save in-bag observations
)




# predict values
df_test$pred_rf <- predict(rf1, df_test)$predictions

df_test = df_test %>%
  mutate(swe_rf = pred_rf*maxv_SNWD)



df_test_filter <- df_test %>%
  dplyr::filter(ID %in% id$ID)

# Convert dataframe of data into list by ID
ls_df_test_lorm <- df_test_filter %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df_test_filter$ID))


lnorm_test_converted_rf = do.call(rbind, lapply(X = ls_df_test_lorm,
                                                 FUN = fit_lnorm, column = "swe_rf" ))



save(lnorm_test_converted_rf, file = "data-raw/RObject/lnorm_test_converted_rf.RData")

# ===========================================================================#
## Fit initial random forest model
# ===========================================================================#

rf_main <- constrained_tree_fit(
  data = df_train, n_trees = 200, lower.ratio = 0.1,
  higher.ratio = 0.55
)



# find the best percentile

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = 14)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)


best_percentile <- mean(unlist(future_lapply(ls_df_train,
  FUN = rf_sd_percentile,
  rf_model = rf_main,
  future.seed = TRUE,
  future.packages = c(
    "fitdistrplus", "tidyverse",
    "matrixStats", "ranger"
  )
)))




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
percentile <- c(0.75, 0.8, 0.9, 0.92, 0.94, 0.96, 0.98, 1)


for (i in percentile) {
  df <- do.call(
    rbind,
    future_lapply(ls_df_test,
      FUN = tree_lnorm_test,
      rf_model = rf_main,
      probs = i,
      future.scheduling = 2, future.seed = TRUE,
      future.packages = c(
        "fitdistrplus", "tidyverse",
        "matrixStats", "ranger"
      )
    )
  )

  # remove decimal
  i <- i * 100

  # assign name to results
  assign(paste0("converted_load_para_q_", i, "_lnorm_rf_test"), df)

  file_name <- paste0("converted_load_para_q_", i, "_lnorm_rf_test")

  # save downloaded data
  save(
    list = paste0("converted_load_para_q_", i, "_lnorm_rf_test"),
    file = paste0("data-raw/RObject/", file_name, ".RData")
  )

  # remove data
  remove(list = paste0("converted_load_para_q_", i, "_lnorm_rf_test"))
  rm(df)
}



tictoc::toc()


## Shut down parallel workers
future::plan("sequential")





# load data
for (i in percentile) {
  # remove decimal
  i <- i * 100

  # create file name
  file_name <- paste0("converted_load_para_q_", i, "_lnorm_rf_test")

  # load file
  load(paste0("data-raw/RObject/", file_name, ".RData"))

  # get station id
  ID <- rownames(get(paste0("converted_load_para_q_", i, "_lnorm_rf_test")))

  # append station id and assign to df
  df <- cbind(ID, get(paste0("converted_load_para_q_", i, "_lnorm_rf_test")))


  df <- as.data.frame(df) %>%
    dplyr::mutate(
      !!paste0("loc_", i, "_cons") := exp(as.numeric(meanlog)),
      !!paste0("sd_", i, "_cons") := exp(as.numeric(sdlog))
    ) %>%
    dplyr::select(ID, !!paste0("loc_", i, "_cons"), !!paste0("sd_", i, "_cons"))

  assign(paste0("converted_load_para_q_", i, "_lnorm_rf_test"), df)
}

load("data-raw/RObject/direct_load_para_lnorm.RData")



direct_load_para_lnorm <- direct_load_para_lnorm %>%
  mutate(
    loc = exp(as.numeric(meanlog)),
    sd = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc, sd)







# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#
percentile <- percentile * 100

# construct names of file
df_names <- paste0("converted_load_para_q_", percentile, "_lnorm_rf_test")

# add direct_load_para_lnorm to the beginning of list
df_list <- mget(df_names)

# loop over dfs and join them
comb_para_bean_lnorm <- direct_load_para_lnorm
for (i in seq_along(df_list)) {
  comb_para_bean_lnorm <- inner_join(
    x = comb_para_bean_lnorm, y = df_list[[i]], by = "ID"
  )
}



#################################################################################
# create relative ratios
#################################################################################
# Create an empty vector to store the  column names to be renamed
cols_to_rename_loc <- c()
cols_to_rename_sd <- c()

# Loop through the column names and add them to the vector
for (i in percentile) {
  col_loc <- paste0("loc_", i, "_cons")
  cols_to_rename_loc <- c(cols_to_rename_loc, col_loc)
}


for (i in percentile) {
  col_sd <- paste0("sd_", i, "_cons")
  cols_to_rename_sd <- c(cols_to_rename_sd, col_sd)
}



# Loop through the column names and rename them
for (col in cols_to_rename_loc) {
  new_col_loc <- paste0("ratio_", col)
  comb_para_bean_lnorm[[new_col_loc]] <- comb_para_bean_lnorm[[col]] / comb_para_bean_lnorm$loc
}


for (col in cols_to_rename_sd) {
  new_col_sd <- paste0("ratio_", col)
  comb_para_bean_lnorm[[new_col_sd]] <- comb_para_bean_lnorm[[col]] / comb_para_bean_lnorm$sd
}

##############################################################################



cols <- paste0("ratio_loc_", percentile, "_cons")
cols <- c(cols, paste0("ratio_sd_", percentile, "_cons"))

for (col in cols) {
   comb_para_bean_lnorm[[col]] <- as.numeric(comb_para_bean_lnorm[[col]])
}

#data_long <- comb_para_bean_lnorm %>% 
#   pivot_longer(cols = cols, names_to = "variable", values_to = "value")

 data_long <- comb_para_bean_lnorm %>% 
    dplyr::select(dplyr::all_of(cols)) %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

#############################################################################


data_long <- data_long %>%
   mutate(method = case_when(
      grepl("\\d+", variable) ~ paste0(gsub("[^0-9]", "", variable), "p of parameters"),
      TRUE ~ NA_character_
   ))


#################################################################################

data_long <- data_long %>%
   mutate(variable = case_when(
      grepl("loc", variable) ~ paste0("mean"),
      TRUE  ~ paste0("sd")
   ))



data_long$method <- as.factor(data_long$method)
levels(data_long$method)

# re-order factor levels
level_names <- paste0(sort(percentile, decreasing = TRUE), "p of parameters")



#re-order factor levels 
data_long$method <- factor(data_long$method, 
                           levels = level_names)

# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.4)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load with different paramter percentile
           (RF on test data) : best percentile 0.75")
