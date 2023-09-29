

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
library(distfixer)

# Source the R file
source(file = "R/fit_lnorm.R")
source(file = "R/fit_gev.R")



# ===========================================================================#
# load data and preprocess
# ===========================================================================#
df <- read.csv("data-raw/data.csv")

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







############################################################################
## STEP 1: RF
############################################################################

# ==========================================================================#
# fit model
# ===========================================================================#

df_train <- df %>%
  filter(data == "Train")


fit_rf <- distfixer::fit_model(
  formula = RATIO ~ logSNWD + SMONTH + D2C +
    logPPTWT + MCMT + MWMT + TD + ELEV,
  data = df_train, method = "rf"
)


df$pred_ratio <- predict(fit_rf, df)$predictions

df <- df %>%
  dplyr::mutate(pred_load = pred_ratio * maxv_SNWD)



# ==========================================================================#
# filter train data again
# ===========================================================================#

df_train <- df %>%
  filter(data == "Train")



# ==========================================================================#
# bias correct : ecdf using just train data
# ===========================================================================#
ecdf_grd_load <- stats::ecdf(df_train$maxv_WESD)

min_grd_load <- min(as.numeric(df_train$maxv_WESD), na.rm = TRUE)
max_grd_load <- max(as.numeric(df_train$maxv_WESD), na.rm = TRUE)
sd_grd_load <- sd(as.numeric(df_train$maxv_WESD), na.rm = TRUE)

in_ecdf_grd_load <- GoFKernel::inverse(
  f = ecdf_grd_load, lower = min_grd_load, upper = max_grd_load
)


# ============================================================================#
# ecdf of  estimated load
# ============================================================================#


ecdf_pred_load <- stats::ecdf(df_train$pred_load)

max_pred_load <- max(df_train$pred_load, na.rm = TRUE)
sd_pred_load <- sd(df_train$pred_load, na.rm = TRUE)




# ==========================================================================#
# filter test data
# ===========================================================================#

df_test <- df %>%
  filter(data == "Test")



# ==========================================================================#
# correct bias
# ===========================================================================#


df_test$adjust_SWE <- NA

for (i in 1:nrow(df_test)) {
  pred_load <- df_test[i, "pred_load"]

  if (pred_load <= max_pred_load) {
    df_test[i, "adjust_SWE"] <- in_ecdf_grd_load(ecdf_pred_load(pred_load))
  } else {
    df_test[i, "adjust_SWE"] <-
      (max_grd_load + (pred_load - max_pred_load) * (sd_grd_load / sd_pred_load))
  }
}


plot(df_test$maxv_WESD - df_test$adjust_SWE)
abline(0, 1)


# ==========================================================================#
# filter test data
# ===========================================================================#


# Filter stations with annual max more than 30
id <- df_test %>%
  dplyr::count(ID) %>%
  dplyr::filter(n >= 30)

df_test_filter <- df_test %>%
  dplyr::filter(ID %in% id$ID)

# Convert dataframe of data into list by ID
ls_df_test <- df_test_filter %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df_test_filter$ID))


ground_load <- do.call(rbind, lapply(X = ls_df_test, FUN = fit_lnorm, column = "maxv_WESD"))

ground_load <- ground_load %>%
  dplyr::mutate(
    mean = exp(as.numeric(meanlog)),
    sd = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean, sd)


est_load <- do.call(rbind, lapply(X = ls_df_test, FUN = fit_lnorm, column = "adjust_SWE"))

est_load <- est_load %>%
  dplyr::mutate(
    mean_est = exp(as.numeric(meanlog)),
    sd_est = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_est, sd_est)




# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#

comb_para_lnorm <- inner_join( x = ground_load, y = est_load, by = c("ID"))



# ===========================================================================#
# create grouping based on ID
# ===========================================================================#

comb_para_lnorm$ID = as.character(comb_para_lnorm$ID )

comb_para_lnorm <- comb_para_lnorm %>%
  mutate(Network = case_when(
    base::grepl("^NY", ID) ~ "NY",
    base::grepl("^USW|^USC", ID) ~ "FOS",
    base::grepl("^USS", ID) ~ "SNOTEL",
    base::grepl("^Maine", ID) ~ "ME",
    base::substr(x = ID, start = nchar(ID) - 3, stop = nchar(ID)) == "SNOW" ~ "SC",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(Network))



# create relative ratios
comb_para_lnorm <- comb_para_lnorm %>%
  mutate(
    ratio_mean = mean_est / mean,
    ratio_sd = sd_est / sd
  )




data_long <- comb_para_lnorm %>%
  pivot_longer(
    cols = c(
      "ratio_mean", "ratio_sd"
    ),
    names_to = "Measure", values_to = "Ratio"
  )



data_long <- data_long %>%
  mutate(method = case_when(
    Measure == "ratio_mean" ~ "ECDF",
    Measure == "ratio_sd" ~ "ECDF"
  ))



data_long <- data_long %>%
  mutate(Measure = case_when(
    Measure == "ratio_mean" ~ "mean",
    Measure == "ratio_sd" ~ "sd"
  ))




# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(data_long, aes(x = Network, y = Ratio, fill = Measure)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.4)) +
  ggtitle("Boxplot ") +
  theme(
    legend.position = c(0.9, 0.25),
    legend.direction = "vertical",
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 35),
    axis.text = element_text(size = 30),
    strip.text = element_text(size = 30)
  )






