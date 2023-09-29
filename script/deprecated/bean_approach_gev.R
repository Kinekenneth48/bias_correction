

##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries and functions
# =============================================================================#
library(ranger)
library(renv)
library(extRemes)
library(tidyverse)
library(matrixStats)
library(future)
library(future.apply)

# Source the R file
source(file = "R/tree_gev_fitting.R")
source(file = "R/fit_distribution.R")


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




##############################################################################
# STEP 1: apply Bean's approach of accounting for variance from RF into
#  distribution fitting
##############################################################################

# ===========================================================================#
# Fit different decision trees and get GEV parameters
# ===========================================================================#


tictoc::tic()

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = 14)


converted_load_para_mean <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = tree_gev_fitting,
    n_trees = 1000,
    mean_para = TRUE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.packages = c("extRemes", "tidyverse", "matrixStats", "ranger")
  )
)



converted_load_para_q_75 <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = tree_gev_fitting,
    n_trees = 1000,
    probs = 0.75,
    mean_para = TRUE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = TRUE,
    future.packages = c("extRemes", "tidyverse", "matrixStats", "ranger")
  )
)



converted_load_para_q_90 <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = tree_gev_fitting,
    n_trees = 1000,
    probs = 0.90,
    mean_para = TRUE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = TRUE,
    future.packages = c("extRemes", "tidyverse", "matrixStats", "ranger")
  )
)

tictoc::toc()


## Shut down parallel workers
future::plan("sequential")



# append station id
ID <- rownames(converted_load_para_mean)
converted_load_para_mean <- cbind(ID, converted_load_para_mean)


# append station id
ID <- rownames(converted_load_para_q_75)
converted_load_para_q_75 <- cbind(ID, converted_load_para_q_75)

# append station id
ID <- rownames(converted_load_para_q_90)
converted_load_para_q_90 <- cbind(ID, converted_load_para_q_90)

# save output
save(converted_load_para_mean, file = "data-raw/RObject/converted_load_para_mean.RData")
save(converted_load_para_q_75, file = "data-raw/RObject/converted_load_para_q_75.RData")
save(converted_load_para_q_90, file = "data-raw/RObject/converted_load_para_q_90.RData")





# ===========================================================================#
# get GEV parameters for direct loads
# ===========================================================================#

direct_load_para <- do.call(rbind, lapply(ls_df,
  FUN = fit_distribution,
  "maxv_WESD",
  dist_name = "GEV"
))


# save output
save(direct_load_para, file = "data-raw/RObject/direct_load_para.RData")




##############################################################################
# STEP 2: Compute relative parameter ratios for converted loads
##############################################################################


# ===========================================================================#
# load data and preprocessing
# ===========================================================================#

# load converted and direct load parameter data
load("data-raw/RObject/direct_load_para.RData")
load("data-raw/RObject/converted_load_para_mean.RData")
load("data-raw/RObject/converted_load_para_q_75.RData")
load("data-raw/RObject/converted_load_para_q_90.RData")


# change class to dataframe
converted_load_para_mean <- as.data.frame(converted_load_para_mean) %>%
  rename(
    loc_mean = location,
    scale_mean = scale,
    shape_mean = shape
  )

converted_load_para_q_75 <- as.data.frame(converted_load_para_q_75) %>%
  rename(
    loc_75 = location,
    scale_75 = scale,
    shape_75 = shape
  )
converted_load_para_q_90 <- as.data.frame(converted_load_para_q_90) %>%
  rename(
    loc_90 = location,
    scale_90 = scale,
    shape_90 = shape
  )

direct_load_para <- direct_load_para %>%
  rename(
    loc = LOC_PRED,
    scale = SCALE_PRED,
    shape = SHAPE_PRED
  ) %>%
  dplyr::select(ID, loc, scale, shape)



# combine distr. para into one dataframe

comb_para_bean <- inner_join(
  x = direct_load_para, y = converted_load_para_q_90,
  by = c("ID")
)

comb_para_bean <- inner_join(
  x = comb_para_bean, y = converted_load_para_q_75,
  by = c("ID")
)

comb_para_bean <- inner_join(
  x = comb_para_bean, y = converted_load_para_mean,
  by = c("ID")
)


comb_para_bean$loc_90 = as.numeric(comb_para_bean$loc_90)
comb_para_bean$scale_90  = as.numeric(comb_para_bean$scale_90 )
comb_para_bean$shape_90 = as.numeric(comb_para_bean$shape_90)
comb_para_bean$loc_75 = as.numeric(comb_para_bean$loc_75)
comb_para_bean$scale_75 = as.numeric(comb_para_bean$scale_75)
comb_para_bean$shape_75 = as.numeric(comb_para_bean$shape_75)
comb_para_bean$loc_mean = as.numeric(comb_para_bean$loc_mean)
comb_para_bean$scale_mean = as.numeric(comb_para_bean$scale_mean)
comb_para_bean$shape_mean = as.numeric(comb_para_bean$shape_mean)

# create relative ratios
comb_para_bean <- comb_para_bean %>%
  mutate(
    ratio_loc_mean = loc_mean / loc,
    ratio_shape_mean = shape_mean / shape,
    ratio_scale_mean = scale_mean / scale,
    ratio_loc_90 = loc_90 / loc,
    ratio_shape_90 = shape_90 / shape,
    ratio_scale_90 = scale_90 / scale,
    ratio_loc_75 = loc_75 / loc,
    ratio_shape_75 = shape_75 / shape,
    ratio_scale_75 = scale_75 / scale
  )



data_long_bean <- comb_para_bean %>% pivot_longer(
  cols = c(
    "ratio_loc_mean", "ratio_shape_mean", "ratio_scale_mean", 
    "ratio_loc_90", "ratio_shape_90", "ratio_scale_90",
    "ratio_loc_75", "ratio_shape_75", "ratio_scale_75"
  ),
  names_to = "variable", values_to = "value"
)



data_long_bean <- data_long_bean %>%
  mutate(method = case_when(
    variable == "ratio_loc_mean" ~ "mean of parameters",
    variable == "ratio_shape_mean" ~ "mean of parameters",
    variable == "ratio_scale_mean" ~ "mean of parameters",
    variable == "ratio_loc_90" ~ "90p of parameters",
    variable == "ratio_shape_90" ~ "90p of parameters",
    variable == "ratio_scale_90" ~ "90p of parameters",
    variable == "ratio_loc_75" ~ "75p of parameters",
    variable == "ratio_shape_75" ~ "75p of parameters",
    variable == "ratio_scale_75" ~ "75p of parameters"
  ))




# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#



ggplot(data_long_bean, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(-2, 2)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load(using Bean's method)")


