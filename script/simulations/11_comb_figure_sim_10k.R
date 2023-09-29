#############################################################################
# This script is created to compares the results of the simulation when data
# generation is at 10,000 observations for Gaussian and uniform data generation.
# This script is the follow-up of 00_simulation_gaussian_model_10k_linear.R and
# 00_simulation_runif_model_10k_linear.R scripts.
##############################################################################


##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries
# =============================================================================#
library(tidyverse)
library(renv)

# =============================================================================#
# load data
# =============================================================================#
load("data-raw/RObject/param_unadjusted_gaussian_10k.RData")
load("data-raw/RObject/param_actual_gaussian_10k.RData")
load("data-raw/RObject/param_adjusted_gaussian_10k.RData")

load("data-raw/RObject/param_unadjusted_runif_10k.RData")
load("data-raw/RObject/param_actual_runif_10k.RData")
load("data-raw/RObject/param_adjusted_runif_10k.RData")





##############################################################################
# STEP 1: clean Gaussian
##############################################################################


# convert to data frame

param_unadjusted_gaussian_10k <- as.data.frame(param_unadjusted_gaussian_10k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_unadjusted = mean, sd_unadjusted = sd) %>%
  dplyr::select(id, mean_unadjusted, sd_unadjusted)


param_adjusted_gaussian_10k <- as.data.frame(param_adjusted_gaussian_10k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_adjusted = mean, sd_adjusted = sd) %>%
  dplyr::select(id, mean_adjusted, sd_adjusted)


param_actual_gaussian_10k <- as.data.frame(param_actual_gaussian_10k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_gaussian_10k <- inner_join(
  x = param_actual_gaussian_10k, y = param_adjusted_gaussian_10k, by = c("id")
)

comb_para_gaussian_10k <- inner_join(
  x = comb_para_gaussian_10k, y = param_unadjusted_gaussian_10k, by = c("id")
)




# create relative ratios
comb_para_gaussian_10k <- comb_para_gaussian_10k %>%
  dplyr::mutate(
    ratio_adjust_mean = mean_adjusted / mean,
    ratio_adjust_sd = sd_adjusted / sd,
    ratio_unadjust_mean = mean_unadjusted / mean,
    ratio_unadjust_sd = sd_unadjusted / sd
  )




data_long_g <- comb_para_gaussian_10k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long_g <- data_long_g %>%
  mutate(Method = case_when(
    Measure == "ratio_unadjust_mean" ~ "Unadjusted Parameters",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted Parameters",
    Measure == "ratio_adjust_mean" ~ "Adjusted Parameters",
    Measure == "ratio_adjust_sd" ~ "Adjusted Parameters",
  ))



data_long_g <- data_long_g %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))


data_long_g <- data_long_g %>%
  dplyr::mutate(Model = "Gaussian")



##############################################################################
# STEP 2: clean uniform
##############################################################################



# convert to data frame

param_unadjusted_runif_10k <- as.data.frame(param_unadjusted_runif_10k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_unadjusted = mean, sd_unadjusted = sd) %>%
  dplyr::select(id, mean_unadjusted, sd_unadjusted)


param_adjusted_runif_10k <- as.data.frame(param_adjusted_runif_10k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_adjusted = mean, sd_adjusted = sd) %>%
  dplyr::select(id, mean_adjusted, sd_adjusted)


param_actual_runif_10k <- as.data.frame(param_actual_runif_10k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_runif_10k <- inner_join(
  x = param_actual_runif_10k, y = param_adjusted_runif_10k, by = c("id")
)

comb_para_runif_10k <- inner_join(
  x = comb_para_runif_10k, y = param_unadjusted_runif_10k, by = c("id")
)



# create relative ratios
comb_para_runif_10k <- comb_para_runif_10k %>%
  dplyr::mutate(
    ratio_adjust_mean = mean_adjusted / mean,
    ratio_adjust_sd = sd_adjusted / sd,
    ratio_unadjust_mean = mean_unadjusted / mean,
    ratio_unadjust_sd = sd_unadjusted / sd
  )




data_long_u <- comb_para_runif_10k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long_u <- data_long_u %>%
  mutate(Method = case_when(
    Measure == "ratio_adjust_mean" ~ "Adjusted Parameters",
    Measure == "ratio_adjust_sd" ~ "Adjusted Parameters",
    Measure == "ratio_unadjust_mean" ~ "Unadjusted Parameters",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted Parameters"
  ))



data_long_u <- data_long_u %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))



data_long_u <- data_long_u %>%
  dplyr::mutate(Model = "Uniform")





##############################################################################
# STEP 3: Make plot
##############################################################################


comb_df <- rbind(data_long_u, data_long_g)
comb_df$Method <- as.factor(comb_df$Method)
levels(comb_df$Method)

# re-order factor levels
comb_df$Method <- factor(comb_df$Method,
  levels = c("Unadjusted Parameters", "Adjusted Parameters")
)




ggplot(comb_df, aes(x = Measure, y = Ratio, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_wrap(~Method, ncol = 2) +
  ylim(c(0.85, 1.05)) +
  ggtitle("Boxplot comparing the relative parameter ratio for the Gaussian model") +
  scale_x_discrete(labels = c("mean" = expression(mu), "sd" = expression(sigma))) +
  theme(
    legend.position = c(0.75, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text.x = element_text(size = 40, margin = margin(t = 10, b = 10)), 
    axis.text.y = element_text(size = 40, margin = margin(r = 10, l = 10)), 
    strip.text = element_text(size = 40)
  )
