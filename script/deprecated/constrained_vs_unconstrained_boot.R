##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries and functions
# =============================================================================#
library(renv)
library(ranger)
library(fitdistrplus)
library(tidyverse)
library(matrixStats)
library(future)
library(future.apply)

# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/fit_distribution.R")
source(file = "R/constrained_tree_lnorm_fitting.R")
source(file = "R/duplicate_row.R")
source(file = "R/constrained_boot_fitting.R")

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


hist(df$RATIO)




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




# ===========================================================================#
# get LNORM parameters for direct loads
# ===========================================================================#

direct_load_para_lnorm <- do.call(rbind, lapply(ls_df,
  FUN = fit_lnorm,
  "maxv_WESD"
))


# save data
save(direct_load_para_lnorm,
  file = "data-raw/RObject/direct_load_para_lnorm.RData"
)





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

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(48879)

converted_load_para_mean_lnorm_const <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = constrained_tree_lnorm_fitting,
    n_trees = 200, lower.ratio = 0.2, higher.ratio = 0.5,
    mean_para = TRUE, constrained_boot = TRUE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = list(
      constrained_boot_fitting = constrained_boot_fitting
    ),
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)

converted_load_para_mean_lnorm <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = constrained_tree_lnorm_fitting,
    n_trees = 200,
    mean_para = TRUE, constrained_boot = FALSE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = list(
      constrained_boot_fitting = constrained_boot_fitting
    ),
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)



converted_load_para_q_90_lnorm_const <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = constrained_tree_lnorm_fitting,
    n_trees = 200, lower.ratio = 0.2, higher.ratio = 0.5,
    probs = 0.90, constrained_boot = TRUE,
    mean_para = FALSE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = list(
      constrained_boot_fitting = constrained_boot_fitting
    ),
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)


converted_load_para_q_90_lnorm <- do.call(
  rbind,
  future_lapply(ls_df,
    FUN = constrained_tree_lnorm_fitting,
    n_trees = 200,
    probs = 0.90, constrained_boot = FALSE,
    mean_para = FALSE,
    future.scheduling = 2,
    future.seed = TRUE,
    future.globals = list(
      constrained_boot_fitting = constrained_boot_fitting
    ),
    future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
  )
)

tictoc::toc()


## Shut down parallel workers
future::plan("sequential")





# append station id
ID <- rownames(converted_load_para_mean_lnorm_const)
converted_load_para_mean_lnorm_const <- cbind(ID, converted_load_para_mean_lnorm_const)


ID <- rownames(converted_load_para_mean_lnorm)
converted_load_para_mean_lnorm <- cbind(ID, converted_load_para_mean_lnorm)


ID <- rownames(converted_load_para_q_90_lnorm_const)
converted_load_para_q_90_lnorm_const <- cbind(ID, converted_load_para_q_90_lnorm_const)

ID <- rownames(converted_load_para_q_90_lnorm)
converted_load_para_q_90_lnorm <- cbind(ID, converted_load_para_q_90_lnorm)





# save data
save(converted_load_para_mean_lnorm_const,
  file = "data-raw/RObject/converted_load_para_mean_lnorm_const.RData"
)

save(converted_load_para_mean_lnorm,
  file = "data-raw/RObject/converted_load_para_mean_lnorm.RData"
)

save(converted_load_para_q_90_lnorm_const,
  file = "data-raw/RObject/converted_load_para_q_90_lnorm_const.RData"
)

save(converted_load_para_q_90_lnorm,
  file = "data-raw/RObject/converted_load_para_q_90_lnorm.RData"
)


##############################################################################
# STEP 2: Compute relative parameter ratios for converted loads
##############################################################################



# change class to dataframe
converted_load_para_mean_lnorm <- as.data.frame(converted_load_para_mean_lnorm) %>%
  dplyr::mutate(
    loc_mean = exp(as.numeric(meanlog)),
    sd_mean = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_mean, sd_mean)

converted_load_para_mean_lnorm_const <- as.data.frame(converted_load_para_mean_lnorm_const) %>%
  dplyr::mutate(
    loc_mean_const = exp(as.numeric(meanlog)),
    sd_mean_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_mean_const, sd_mean_const)


converted_load_para_q_90_lnorm_const <- as.data.frame(converted_load_para_q_90_lnorm_const) %>%
  mutate(
    loc_90_const = exp(as.numeric(meanlog)),
    sd_90_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_90_const, sd_90_const)



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






# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_bean_lnorm <- inner_join(
  x = direct_load_para_lnorm, y = converted_load_para_mean_lnorm,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_mean_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_90_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_90_lnorm,
  by = c("ID")
)


# create relative ratios
comb_para_bean_lnorm <- comb_para_bean_lnorm %>%
  mutate(
    ratio_loc_mean = loc_mean / loc,
    ratio_sd_mean = sd_mean / sd,
    ratio_loc_mean_const = loc_mean_const / loc,
    ratio_sd_mean_const = sd_mean_const / sd,
    ratio_loc_90 = loc_90 / loc,
    ratio_sd_90 = sd_90 / sd,
    ratio_loc_90_const = loc_90_const / loc,
    ratio_sd_90_const = sd_90_const / sd

  )



data_long_bean <- comb_para_bean_lnorm %>%
  pivot_longer(
    cols = c(
      "ratio_loc_mean", "ratio_sd_mean",
      "ratio_loc_mean_const", "ratio_sd_mean_const",
      "ratio_loc_90", "ratio_sd_90",
      "ratio_loc_90_const", "ratio_sd_90_const"
    ),
    names_to = "variable", values_to = "value"
  )



data_long_bean <- data_long_bean %>%
  mutate(method = case_when(
    variable == "ratio_loc_mean" ~ "mean of parameters",
    variable == "ratio_sd_mean" ~ "mean of parameters",
    variable == "ratio_loc_mean_const" ~ "mean of parameters",
    variable == "ratio_sd_mean_const" ~ "mean of parameters",
    variable == "ratio_loc_90" ~ "90p of parameters",
    variable == "ratio_sd_90" ~ "90p of parameters",
    variable == "ratio_loc_90_const" ~ "90p of parameters",
    variable == "ratio_sd_90_const" ~ "90p of parameters"
  ))




# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#



ggplot(data_long_bean, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.6)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load
          (using constrained bootstrapping vs unconstrained bootstrapping)")
