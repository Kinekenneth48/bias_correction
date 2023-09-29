
#############################################################################
############ Step 0: Load libraries and data
#############################################################################

# ===========================================================================#
# load libraries
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
library(data.table)

# ===========================================================================#
# load functions
# ===========================================================================#
# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/make_predictions.R")
source(file = "R/combine_dt_to_rf_object.R")
source(file = "R/keep_max_snwd.R")


# ===========================================================================#
# load data
# ===========================================================================#
load("data-raw/RObject/df_fos_coop_with_imputed_wesd_part2_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd_part1_nosnwd_impute.RData")
load("data-raw/RObject/df_sntl_with_imputed_wesd_nosnwd_impute.RData")

df <- read.csv("data-raw/data.csv") # Read the data from data.csv file
#df_jess <- read.csv("data-raw/data.csv")
#unique_ids <- unique(df_jess$ID)

df_fos_coop1 = df_fos_coop_with_imputed_wesd_part1_nosnwd_impute
df_fos_coop2 = df_fos_coop_with_imputed_wesd_part2_nosnwd_impute
df_sntl= df_sntl_with_imputed_wesd

remove(df_fos_coop_with_imputed_wesd_part1_nosnwd_impute)
remove(df_fos_coop_with_imputed_wesd_part2_nosnwd_impute)
remove(df_sntl_with_imputed_wesd)
# ===========================================================================#
# data cleaning
# ===========================================================================#
id <- df_jess %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 30)

df_sntl = df_sntl   %>%
  dplyr::filter(ID %in% id$ID)

df_fos_coop1 = df_fos_coop1   %>%
  dplyr::filter(ID %in% id$ID)

df_fos_coop2 = df_fos_coop2   %>%
  dplyr::filter(ID %in% id$ID)

# CHECK TEST
diff_rows <- df_fos_coop_with_imputed_wesd_part1_nosnwd_impute[
  df_fos_coop_with_imputed_wesd_part1_nosnwd_impute$WESD !=
    df_fos_coop_with_imputed_wesd_part1_nosnwd_impute$WESD_I,
]



# ===========================================================================#
# take max of WESD
# ===========================================================================#

#keep pairs of "WESD", "WESD_I"
df_fos_coop_with_imputed_wesd_part1_nosnwd_impute <- 
  df_fos_coop_with_imputed_wesd_part1_nosnwd_impute[complete.cases(df_fos_coop_with_imputed_wesd_part1_nosnwd_impute[c("WESD", "WESD_I")]), ]


df_fos_coop_with_imputed_wesd_part2_nosnwd_impute <- 
  df_fos_coop_with_imputed_wesd_part2_nosnwd_impute[complete.cases(df_fos_coop_with_imputed_wesd_part2_nosnwd_impute[c("WESD", "WESD_I")]), ]


df_sntl_with_imputed_wesd <- 
  df_sntl_with_imputed_wesd[complete.cases(df_sntl_with_imputed_wesd[c("WESD", "WESD_I")]), ]


df_fos_coop_max1 <-
  keep_max_snwd(df_fos_coop_with_imputed_wesd_part1_nosnwd_impute)

df_fos_coop_max2 <-
  keep_max_snwd(df_fos_coop2)

df_sntl_max <-
  keep_max_snwd(df_sntl_with_imputed_wesd)




df_fos_coop = rbind(df_fos_coop_max1, df_fos_coop_max2)


df_fos_coop_sntl = rbind(df_fos_coop, df_sntl_max)




# Filter stations with annual max more than 30
id <- df_fos_coop_sntl %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 30)

df_fos_coop_sntl <- df_fos_coop_sntl %>%
  dplyr::filter(ID %in% id$ID)




# Convert dataframe of data into list by ID
ls_df <- df %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df$ID))
