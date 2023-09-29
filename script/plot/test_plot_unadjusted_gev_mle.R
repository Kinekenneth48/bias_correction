#############################################################################
# This script is created to visualize the GEV(mle) distr parameters when
# bias is not corrected for the test data.
##############################################################################


library(tidyverse)
library(renv)

# load data
load("data-raw/RObject/gev_test_unadjusted_svr.RData")
load("data-raw/RObject/gev_test_unadjusted_gbm.RData")
load("data-raw/RObject/gev_test_unadjusted_rf.RData")
load("data-raw/RObject/test_direct_gev_para.RData")




gev_test_unadjusted_svr <- gev_test_unadjusted_svr %>%
  mutate(
    ID = rownames(.),
    loc_svr = loc,
    scale_svr = scale,
    shape_svr = shape
  ) %>%
  dplyr::select(ID, loc_svr, scale_svr, shape_svr)


gev_test_unadjusted_gbm <- gev_test_unadjusted_gbm %>%
  mutate(
    ID = rownames(.),
    loc_gbm = loc,
    scale_gbm = scale,
    shape_gbm = shape
  ) %>%
  dplyr::select(ID, loc_gbm, scale_gbm, shape_gbm)

gev_test_unadjusted_rf <- gev_test_unadjusted_rf %>%
  mutate(
    ID = rownames(.),
    loc_rf = loc,
    scale_rf = scale,
    shape_rf = shape
  ) %>%
  dplyr::select(ID, loc_rf, scale_rf, shape_rf)


test_direct_gev_para <- test_direct_gev_para %>%
  mutate(ID = rownames(.))




# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_gev <- inner_join(
  x = test_direct_gev_para, y = gev_test_unadjusted_rf,
  by = c("ID")
)

comb_para_gev <- inner_join(
  x = comb_para_gev, y = gev_test_unadjusted_gbm,
  by = c("ID")
)

comb_para_gev <- inner_join(
  x = comb_para_gev, y = gev_test_unadjusted_svr,
  by = c("ID")
)


# ===========================================================================#
# create grouping based on ID
# ===========================================================================#

comb_para_gev$ID <- as.character(comb_para_gev$ID)

comb_para_gev <- comb_para_gev %>%
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
comb_para_gev <- comb_para_gev %>%
  mutate(
    ratio_loc_rf = loc_rf / loc,
    ratio_scale_rf = scale_rf / scale,
    rc_shape_rf = shape_rf - shape / abs(shape),
    ratio_loc_svr = loc_svr / loc,
    ratio_scale_svr = scale_svr / scale,
    rc_shape_svr = shape_svr - shape / abs(shape),
    ratio_loc_gbm = loc_gbm / loc,
    ratio_scale_gbm = scale_gbm / scale,
    rc_shape_gbm = shape_gbm - shape / abs(shape),
  )





data_long <- comb_para_gev %>%
  pivot_longer(
    cols = c(
      "ratio_loc_rf", "ratio_scale_rf", "rc_shape_rf",
      "ratio_loc_svr", "ratio_scale_svr", "rc_shape_svr",
      "ratio_loc_gbm", "ratio_scale_gbm", "rc_shape_gbm"
    ),
    names_to = "Measure", values_to = "Ratio"
  )



data_long <- data_long %>%
  mutate(method = case_when(
    Measure == "ratio_loc_rf" ~ "RF",
    Measure == "ratio_scale_rf" ~ "RF",
    Measure == "rc_shape_rf" ~ "RF",
    Measure == "ratio_loc_svr" ~ "SVR",
    Measure == "ratio_scale_svr" ~ "SVR",
    Measure == "rc_shape_svr" ~ "SVR",
    Measure == "ratio_loc_gbm" ~ "GBM",
    Measure == "ratio_scale_gbm" ~ "GBM",
    Measure == "rc_shape_gbm" ~ "GBM"
  ))



data_long <- data_long %>%
  mutate(Measure = case_when(
    Measure == "ratio_loc_rf" ~ "location",
    Measure == "ratio_loc_svr" ~ "location",
    Measure == "ratio_loc_gbm" ~ "location",
    Measure == "ratio_scale_rf" ~ "scale",
    Measure == "ratio_scale_svr" ~ "scale",
    Measure == "ratio_scale_gbm" ~ "scale",
    Measure == "rc_shape_rf" ~ "shape",
    Measure == "rc_shape_svr" ~ "shape",
    Measure == "rc_shape_gbm" ~ "shape"
  ))


loc_scale_data <- data_long %>%
  dplyr::filter(!(Measure == "shape"))



# data_long$method <- as.factor(data_long$method)
# levels(data_long$method)

loc_scale_data_unadjusted <- loc_scale_data %>%
  mutate(condition = "Unadjusted")

shape_data <- data_long %>%
  dplyr::filter((Measure == "shape"))

shape_data_unadjusted <- shape_data %>%
  mutate(condition = "Unadjusted")

save(loc_scale_data_unadjusted, file = "data-raw/RObject/loc_scale_data_unadjusted.RData")
save(shape_data_unadjusted, file = "data-raw/RObject/shape_data_unadjusted.RData")

# ===========================================================================#
# relative ratio boxplot
# ===========================================================================#


ggplot(loc_scale_data, aes(x = Network, y = Ratio, fill = Measure)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(0.6, 1.4)) +
  ggtitle("Boxplot ") +
  theme(
    legend.position = c(0.85, 0.1),
    legend.direction = "vertical",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 40),
    strip.text = element_text(size = 40)
  )




ggplot(shape_data, aes(x = Network, y = Ratio, fill = Measure)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  facet_wrap(~method, ncol = 2) +
  ylim(c(-1.5, 1.5)) +
  ggtitle("Boxplot ") +
  theme(
    legend.position = c(0.85, 0.1),
    legend.direction = "vertical",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 40),
    strip.text = element_text(size = 40)
  ) +
  ylab("Relative change")
