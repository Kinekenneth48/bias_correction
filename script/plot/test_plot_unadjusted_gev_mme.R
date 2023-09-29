#############################################################################
# This script is created to visualize the GEV(mme) distr parameters when
# bias is not corrected for the test data.
##############################################################################
library(tidyverse)
library(renv)

# from test_distrfixer
# load("data-raw/RObject/lnorm_test_converted_rf.RData")
# load("data-raw/RObject/lnorm_test_converted_svr.RData")
# load("data-raw/RObject/lnorm_test_converted_gbm.RData")
# load("data-raw/RObject/lnorm_test_actual.RData")
# load("data-raw/RObject/test_direct_lnorm_para.RData")


load("data-raw/RObject/lnorm_test_unadjusted_rf_mme.RData")
load("data-raw/RObject/test_direct_lnorm_para_mme.RData")
load("data-raw/RObject/lnorm_test_unadjusted_gbm_mme.RData")
load("data-raw/RObject/lnorm_test_unadjusted_svr_mme.RData")

lnorm_test_unadjusted_rf_mme <- lnorm_test_unadjusted_rf_mme %>%
  mutate(
    mean_rf = exp(as.numeric(meanlog)),
    sd_rf = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_rf, sd_rf)


lnorm_test_unadjusted_svr_mme <- lnorm_test_unadjusted_svr_mme %>%
  mutate(
    mean_svr = exp(as.numeric(meanlog)),
    sd_svr = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_svr, sd_svr)

test_direct_lnorm_para_mme <- test_direct_lnorm_para_mme %>%
  mutate(
    mean = exp(as.numeric(meanlog)),
    sd = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean, sd)


lnorm_test_unadjusted_gbm_mme <- lnorm_test_unadjusted_gbm_mme %>%
  mutate(
    mean_gbm = exp(as.numeric(meanlog)),
    sd_gbm = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_gbm, sd_gbm)



# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_lnorm <- inner_join(
  x = test_direct_lnorm_para_mme, y = lnorm_test_unadjusted_gbm_mme,
  by = c("ID")
)



comb_para_lnorm <- inner_join(
  x = comb_para_lnorm, y = lnorm_test_unadjusted_svr_mme,
  by = c("ID")
)

comb_para_lnorm <- inner_join(
  x = comb_para_lnorm, y = lnorm_test_unadjusted_rf_mme,
  by = c("ID")
)


# ===========================================================================#
# create grouping based on ID
# ===========================================================================#

comb_para_lnorm$ID <- as.character(comb_para_lnorm$ID)

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
    ratio_mean_rf = mean_rf / mean,
    ratio_sd_rf = sd_rf / sd,
    ratio_mean_svr = mean_svr / mean,
    ratio_sd_svr = sd_svr / sd,
    ratio_mean_gbm = mean_gbm / mean,
    ratio_sd_gbm = sd_gbm / sd
  )





data_long <- comb_para_lnorm %>%
  pivot_longer(
    cols = c(
      "ratio_mean_rf", "ratio_sd_rf", "ratio_mean_svr",
      "ratio_sd_svr", "ratio_mean_gbm", "ratio_sd_gbm"
    ),
    names_to = "Measure", values_to = "Ratio"
  )



data_long <- data_long %>%
  mutate(method = case_when(
    Measure == "ratio_mean_rf" ~ "RF",
    Measure == "ratio_sd_rf" ~ "RF",
    Measure == "ratio_mean_svr" ~ "SVR",
    Measure == "ratio_sd_svr" ~ "SVR",
    Measure == "ratio_mean_gbm" ~ "GBM",
    Measure == "ratio_sd_gbm" ~ "GBM"
  ))



data_long <- data_long %>%
  mutate(Measure = case_when(
    Measure == "ratio_mean_rf" ~ "mean",
    Measure == "ratio_sd_rf" ~ "sd",
    Measure == "ratio_mean_svr" ~ "mean",
    Measure == "ratio_sd_svr" ~ "sd",
    Measure == "ratio_mean_gbm" ~ "mean",
    Measure == "ratio_sd_gbm" ~ "sd"
  ))



data_long$method <- as.factor(data_long$method)
levels(data_long$method)



#
# # re-order factor levels
# data_long$method <- factor(data_long$method,
#                            levels = c(
#                              "100p of parameters", "98p of parameters",
#                              "96p of parameters", "94p of parameters",
#                              "92p of parameters", "90p of parameters",
#                              "38p of parameters", "85p of parameters"
#                            )
# )



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
    legend.position = c(0.75, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 40),
    strip.text = element_text(size = 40)
  )
