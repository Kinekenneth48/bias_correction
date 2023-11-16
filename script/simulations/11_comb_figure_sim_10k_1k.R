#############################################################################
# This script is created to compares the results of the simulation when data
# generation is at 1,000 and 10,000 observations for Gaussian and uniform data generation.
# This script is the follow-up of 00_simulation_gaussian_model_1k_linear.R,
# 00_simulation_gaussian_model_10k_linear.R , 00_simulation_runif_model_10k_linear.R and
# 00_simulation_runif_model_1k_linear.R scripts.
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
base::load("data-raw/RObject/param_unadjusted_gaussian_10k.RData")
base::load("data-raw/RObject/param_actual_gaussian_10k.RData")
base::load("data-raw/RObject/param_adjusted_gaussian_10k.RData")

base::load("data-raw/RObject/param_unadjusted_runif_10k.RData")
base::load("data-raw/RObject/param_actual_runif_10k.RData")
base::load("data-raw/RObject/param_adjusted_runif_10k.RData")

base::load("data-raw/RObject/param_unadjusted_gaussian_1k.RData")
base::load("data-raw/RObject/param_actual_gaussian_1k.RData")
base::load("data-raw/RObject/param_adjusted_gaussian_1k.RData")

base::load("data-raw/RObject/param_unadjusted_runif_1k.RData")
base::load("data-raw/RObject/param_actual_runif_1k.RData")
base::load("data-raw/RObject/param_adjusted_runif_1k.RData")






##############################################################################
# STEP 1: clean Gaussian - 10k
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




data_long_g_10k <- comb_para_gaussian_10k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long_g_10k <- data_long_g_10k %>%
  mutate(Method = case_when(
    Measure == "ratio_unadjust_mean" ~ "Unadjusted",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted",
    Measure == "ratio_adjust_mean" ~ "Adjusted",
    Measure == "ratio_adjust_sd" ~ "Adjusted",
  ))



data_long_g_10k <- data_long_g_10k %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))


data_long_g_10k <- data_long_g_10k %>%
  dplyr::mutate(Model = "Gaussian", size = "n=10,000")



##############################################################################
# STEP 2: clean uniform - 10k
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




data_long_u_10k <- comb_para_runif_10k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long_u_10k <- data_long_u_10k %>%
  mutate(Method = case_when(
    Measure == "ratio_adjust_mean" ~ "Adjusted",
    Measure == "ratio_adjust_sd" ~ "Adjusted",
    Measure == "ratio_unadjust_mean" ~ "Unadjusted",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted"
  ))



data_long_u_10k <- data_long_u_10k %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))



data_long_u_10k <- data_long_u_10k %>%
  dplyr::mutate(Model = "Uniform", size = "n=10,000")








##############################################################################
# STEP 3: clean Gaussian - 1k
##############################################################################


# convert to data frame

param_unadjusted_gaussian_1k <- as.data.frame(param_unadjusted_gaussian_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_unadjusted = mean, sd_unadjusted = sd) %>%
  dplyr::select(id, mean_unadjusted, sd_unadjusted)


param_adjusted_gaussian_1k <- as.data.frame(param_adjusted_gaussian_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_adjusted = mean, sd_adjusted = sd) %>%
  dplyr::select(id, mean_adjusted, sd_adjusted)


param_actual_gaussian_1k <- as.data.frame(param_actual_gaussian_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_gaussian_1k <- inner_join(
  x = param_actual_gaussian_1k, y = param_adjusted_gaussian_1k, by = c("id")
)

comb_para_gaussian_1k <- inner_join(
  x = comb_para_gaussian_1k, y = param_unadjusted_gaussian_1k, by = c("id")
)




# create relative ratios
comb_para_gaussian_1k <- comb_para_gaussian_1k %>%
  dplyr::mutate(
    ratio_adjust_mean = mean_adjusted / mean,
    ratio_adjust_sd = sd_adjusted / sd,
    ratio_unadjust_mean = mean_unadjusted / mean,
    ratio_unadjust_sd = sd_unadjusted / sd
  )




data_long_g_1k <- comb_para_gaussian_1k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long_g_1k <- data_long_g_1k %>%
  mutate(Method = case_when(
    Measure == "ratio_unadjust_mean" ~ "Unadjusted",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted",
    Measure == "ratio_adjust_mean" ~ "Adjusted",
    Measure == "ratio_adjust_sd" ~ "Adjusted",
  ))



data_long_g_1k <- data_long_g_1k %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))


data_long_g_1k <- data_long_g_1k %>%
  dplyr::mutate(Model = "Gaussian", size = "n=1,000")



##############################################################################
# STEP 4: clean uniform -1k
##############################################################################



# convert to data frame

param_unadjusted_runif_1k <- as.data.frame(param_unadjusted_runif_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_unadjusted = mean, sd_unadjusted = sd) %>%
  dplyr::select(id, mean_unadjusted, sd_unadjusted)


param_adjusted_runif_1k <- as.data.frame(param_adjusted_runif_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::rename(mean_adjusted = mean, sd_adjusted = sd) %>%
  dplyr::select(id, mean_adjusted, sd_adjusted)


param_actual_runif_1k <- as.data.frame(param_actual_runif_1k) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_runif_1k <- inner_join(
  x = param_actual_runif_1k, y = param_adjusted_runif_1k, by = c("id")
)

comb_para_runif_1k <- inner_join(
  x = comb_para_runif_1k, y = param_unadjusted_runif_1k, by = c("id")
)



# create relative ratios
comb_para_runif_1k <- comb_para_runif_1k %>%
  dplyr::mutate(
    ratio_adjust_mean = mean_adjusted / mean,
    ratio_adjust_sd = sd_adjusted / sd,
    ratio_unadjust_mean = mean_unadjusted / mean,
    ratio_unadjust_sd = sd_unadjusted / sd
  )




data_long_u_1k <- comb_para_runif_1k %>%
  pivot_longer(
    cols = c(
      "ratio_adjust_mean", "ratio_adjust_sd", "ratio_unadjust_mean",
      "ratio_unadjust_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )


data_long_u_1k <- data_long_u_1k %>%
  mutate(Method = case_when(
    Measure == "ratio_adjust_mean" ~ "Adjusted",
    Measure == "ratio_adjust_sd" ~ "Adjusted",
    Measure == "ratio_unadjust_mean" ~ "Unadjusted",
    Measure == "ratio_unadjust_sd" ~ "Unadjusted"
  ))



data_long_u_1k <- data_long_u_1k %>%
  mutate(Measure = case_when(
    Measure == "ratio_adjust_mean" ~ "mean",
    Measure == "ratio_unadjust_mean" ~ "mean",
    Measure == "ratio_adjust_sd" ~ "sd",
    Measure == "ratio_unadjust_sd" ~ "sd"
  ))



data_long_u_1k <- data_long_u_1k %>%
  dplyr::mutate(Model = "Uniform", size = "n=1,000")






##############################################################################
# STEP 5: Make plot
##############################################################################


comb_df <- rbind(data_long_u_10k, data_long_g_10k, data_long_u_1k, data_long_g_1k)
comb_df$Method <- as.factor(comb_df$Method)
comb_df$size <- as.factor(comb_df$size)
levels(comb_df$Method)
levels(comb_df$size)

# re-order factor levels
comb_df$Method <- factor(comb_df$Method,
  levels = c("Unadjusted", "Adjusted")
)

comb_df$size <- factor(comb_df$size,
  levels = c("n=10,000", "n=1,000")
)



ggplot(comb_df, aes(x = Measure, y = Ratio, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", size = 1.1) +
  facet_grid(rows = vars(size), cols = vars(Method)) +
  ylim(c(0.85, 1.05)) +
  ggtitle("Boxplot comparing the relative parameter ratio for the Gaussian model") +
  scale_x_discrete(labels = c("mean" = expression(mu), "sd" = expression(sigma))) +
  xlab(" " ) + 
  theme(
    legend.position = c(0.5, -0.112),
    legend.direction = "horizontal",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text.x = element_text(size = 40, margin = margin(t = 10, b = 10)),
    axis.text.y = element_text(size = 40, margin = margin(r = 10, l = 10)),
    strip.text = element_text(size = 40)
  )
