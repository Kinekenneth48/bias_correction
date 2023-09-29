#############################################################################
# This script is created to visualize the log normal distr parameters when
# bias is corrected for the test data.
##############################################################################
library(renv)
library(tidyverse)
# load data
# load("data-raw/RObject/converted_load_para_q_30_lnorm_svr.RData")
# load("data-raw/RObject/converted_load_para_q_75_lnorm_rf_test.RData")
# load("data-raw/RObject/converted_load_para_q_38_lnorm_gbm.RData")
# load("data-raw/RObject/direct_load_para_lnorm.RData")


load("data-raw/RObject/test_direct_lnorm_para.RData")
load("data-raw/RObject/test_svr_lnorm_para.RData")
load("data-raw/RObject/test_gbm_lnorm_para.RData")
load("data-raw/RObject/test_rf_lnorm_para.RData")

# append station id
ID <- rownames(test_svr_lnorm_para)
test_svr_lnorm_para <- cbind(ID, test_svr_lnorm_para)

ID <- rownames(test_rf_lnorm_para)
test_rf_lnorm_para <- cbind(ID, test_rf_lnorm_para)

ID <- rownames(test_gbm_lnorm_para)
test_gbm_lnorm_para <- cbind(ID, test_gbm_lnorm_para)




# change class to dataframe

test_svr_lnorm_para <- as.data.frame(test_svr_lnorm_para) %>%
  mutate(
    mean_svr = exp(as.numeric(meanlog)),
    sd_svr = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_svr, sd_svr)

test_rf_lnorm_para <- as.data.frame(test_rf_lnorm_para) %>%
  mutate(
    mean_rf = exp(as.numeric(meanlog)),
    sd_rf = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_rf, sd_rf)


test_gbm_lnorm_para <- as.data.frame(test_gbm_lnorm_para) %>%
  mutate(
    mean_gbm = exp(as.numeric(meanlog)),
    sd_gbm = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean_gbm, sd_gbm)


test_direct_lnorm_para <- test_direct_lnorm_para %>%
  mutate(
    mean = exp(as.numeric(meanlog)),
    sd = exp(as.numeric(sdlog))
  ) %>%
  dplyr::select(ID, mean, sd)


# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_lnorm <- inner_join(
  x = test_direct_lnorm_para, y = test_svr_lnorm_para,
  by = c("ID")
)


comb_para_lnorm <- inner_join(
  x = comb_para_lnorm, y = test_gbm_lnorm_para,
  by = c("ID")
)


comb_para_lnorm <- inner_join(
  x = comb_para_lnorm, y = test_rf_lnorm_para,
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



data_long_adjusted <- data_long %>%
  mutate(condition = "Adjusted")

save(data_long_adjusted, file = "data-raw/Robject/data_long_adjusted.RData")


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
