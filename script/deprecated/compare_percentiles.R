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
set.seed(1421425)

# specify percentiles
percentile <- c(0.8, 0.85, 0.9, 0.92, 0.94, 0.96, 0.98, 1)


for (i in percentile) {
  df <- do.call(
    rbind,
    future_lapply(ls_df,
      FUN = constrained_tree_lnorm_fitting,
      n_trees = 200, lower.ratio = 0.2, higher.ratio = 0.5,
      probs = i, constrained_boot = TRUE,
      mean_para = FALSE,
      future.scheduling = 2,
      future.seed = TRUE,
      future.globals = list(
        constrained_boot_fitting = constrained_boot_fitting
      ),
      future.packages = c("fitdistrplus", "tidyverse", "matrixStats", "ranger")
    )
  )

  # remove decimal
  i <- i * 100

  # assign name to results
  assign(paste0("converted_load_para_q_", i, "_lnorm_const"), df)

  file_name <- paste0("converted_load_para_q_", i, "_lnorm_const")

  # save downloaded data
  save(
    list = paste0("converted_load_para_q_", i, "_lnorm_const"),
    file = paste0("data-raw/RObject/", file_name, ".RData")
  )

  # remove data
  remove(list = paste0("converted_load_para_q_", i, "_lnorm_const"))
  rm(df)
}



tictoc::toc()


## Shut down parallel workers
future::plan("sequential")




# load data
load("data-raw/RObject/converted_load_para_q_80_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_85_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_90_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_92_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_94_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_96_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_98_lnorm_const.RData")
load("data-raw/RObject/converted_load_para_q_100_lnorm_const.RData")
load("data-raw/RObject/direct_load_para_lnorm.RData")



# append station id
ID <- rownames(converted_load_para_q_80_lnorm_const)
converted_load_para_q_80_lnorm_const <- cbind(ID, converted_load_para_q_80_lnorm_const)

ID <- rownames(converted_load_para_q_85_lnorm_const)
converted_load_para_q_85_lnorm_const <- cbind(ID, converted_load_para_q_85_lnorm_const)

ID <- rownames(converted_load_para_q_90_lnorm_const)
converted_load_para_q_90_lnorm_const <- cbind(ID, converted_load_para_q_90_lnorm_const)

ID <- rownames(converted_load_para_q_92_lnorm_const)
converted_load_para_q_92_lnorm_const <- cbind(ID, converted_load_para_q_92_lnorm_const)

ID <- rownames(converted_load_para_q_94_lnorm_const)
converted_load_para_q_94_lnorm_const <- cbind(ID, converted_load_para_q_94_lnorm_const)

ID <- rownames(converted_load_para_q_96_lnorm_const)
converted_load_para_q_96_lnorm_const <- cbind(ID, converted_load_para_q_96_lnorm_const)

ID <- rownames(converted_load_para_q_98_lnorm_const)
converted_load_para_q_98_lnorm_const <- cbind(ID, converted_load_para_q_98_lnorm_const)

ID <- rownames(converted_load_para_q_100_lnorm_const)
converted_load_para_q_100_lnorm_const <- cbind(ID, converted_load_para_q_100_lnorm_const)





##############################################################################
# STEP 2: Compute relative parameter ratios for converted loads
##############################################################################



# change class to dataframe

converted_load_para_q_80_lnorm_const <- as.data.frame(converted_load_para_q_80_lnorm_const) %>%
  mutate(
    loc_80_const = exp(as.numeric(meanlog)),
    sd_80_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_80_const, sd_80_const)

converted_load_para_q_85_lnorm_const <- as.data.frame(converted_load_para_q_85_lnorm_const) %>%
  mutate(
    loc_85_const = exp(as.numeric(meanlog)),
    sd_85_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_85_const, sd_85_const)

converted_load_para_q_90_lnorm_const <- as.data.frame(converted_load_para_q_90_lnorm_const) %>%
  mutate(
    loc_90_const = exp(as.numeric(meanlog)),
    sd_90_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_90_const, sd_90_const)

converted_load_para_q_92_lnorm_const <- as.data.frame(converted_load_para_q_92_lnorm_const) %>%
  mutate(
    loc_92_const = exp(as.numeric(meanlog)),
    sd_92_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_92_const, sd_92_const)

converted_load_para_q_94_lnorm_const <- as.data.frame(converted_load_para_q_94_lnorm_const) %>%
  mutate(
    loc_94_const = exp(as.numeric(meanlog)),
    sd_94_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_94_const, sd_94_const)

converted_load_para_q_96_lnorm_const <- as.data.frame(converted_load_para_q_96_lnorm_const) %>%
  mutate(
    loc_96_const = exp(as.numeric(meanlog)),
    sd_96_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_96_const, sd_96_const)

converted_load_para_q_98_lnorm_const <- as.data.frame(converted_load_para_q_98_lnorm_const) %>%
  mutate(
    loc_98_const = exp(as.numeric(meanlog)),
    sd_98_const = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, loc_98_const, sd_98_const)

converted_load_para_q_100_lnorm_const <- as.data.frame(converted_load_para_q_100_lnorm_const) %>%
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
  x = direct_load_para_lnorm, y = converted_load_para_q_100_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_98_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_96_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_94_lnorm_const,
  by = c("ID")
)


comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_92_lnorm_const,
  by = c("ID")
)


comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_90_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_85_lnorm_const,
  by = c("ID")
)

comb_para_bean_lnorm <- inner_join(
  x = comb_para_bean_lnorm, y = converted_load_para_q_80_lnorm_const,
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
    ratio_loc_80_const = loc_80_const / loc,
    ratio_sd_80_const = sd_80_const / sd
  )





data_long <- comb_para_bean_lnorm %>% pivot_longer(
  cols = c(
    "ratio_loc_100_const" , "ratio_sd_100_const" , "ratio_loc_98_const" ,
    "ratio_sd_98_const" ,"ratio_loc_96_const" , "ratio_sd_96_const" ,
    "ratio_loc_94_const" , "ratio_sd_94_const" ,"ratio_loc_92_const" ,
    "ratio_sd_92_const" ,"ratio_loc_90_const" , "ratio_sd_90_const" ,
    "ratio_loc_85_const" , "ratio_sd_85_const" ,"ratio_loc_80_const" ,
    "ratio_sd_80_const" 
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
    variable == "ratio_loc_80_const" ~ "80p of parameters",
    variable == "ratio_sd_80_const" ~ "80p of parameters"
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
    variable == "ratio_loc_80_const" ~ "mean",
    variable == "ratio_sd_100_const" ~ "sd",
    variable == "ratio_sd_98_const" ~ "sd",
    variable == "ratio_sd_96_const" ~ "sd",
    variable == "ratio_sd_94_const" ~ "sd",
    variable == "ratio_sd_92_const" ~ "sd",
    variable == "ratio_sd_90_const" ~ "sd",
    variable == "ratio_sd_85_const" ~ "sd",
    variable == "ratio_sd_80_const" ~ "sd"
  ))


data_long$method = as.factor(data_long$method)
levels(data_long$method)

#re-order factor levels 
data_long$method <- factor(data_long$method, 
                levels = c("100p of parameters", "98p of parameters" , 
                           "96p of parameters" , "94p of parameters",
                           "92p of parameters" , "90p of parameters",
                           "80p of parameters" , "85p of parameters" ))

# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.4)) +
  ggtitle("Boxplot comparing the relative parameter ratio of direct load vs converted load with different paramter percentile")







