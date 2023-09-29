library(tidyverse)
library(renv)

# from test_distrfixer_gev
load("data-raw/RObject/test_rf_gev_para.RData")
load("data-raw/RObject/test_gbm_gev_para.RData")
load("data-raw/RObject/test_svr_gev_para.RData")
load("data-raw/RObject/test_direct_gev_para.RData")




test_svr_gev_para <- as.data.frame( test_svr_gev_para) %>%
  dplyr::mutate(ID = rownames(.),
         loc_svr = location,
         scale_svr = scale,
         shape_svr = shape
  ) %>%
  dplyr::select(ID, loc_svr, scale_svr, shape_svr)


test_gbm_gev_para <- as.data.frame(test_gbm_gev_para) %>%
  dplyr::mutate(ID = rownames(.),
         loc_gbm = location,
         scale_gbm = scale,
         shape_gbm = shape
  ) %>%
  dplyr::select(ID, loc_gbm, scale_gbm, shape_gbm)

test_rf_gev_para <- as.data.frame(test_rf_gev_para) %>%
  dplyr::mutate(ID = rownames(.),
         loc_rf = location,
         scale_rf = scale,
         shape_rf = shape
  ) %>%
  dplyr::select(ID, loc_rf, scale_rf, shape_rf)


test_direct_gev_para <- test_direct_gev_para %>%
  dplyr::mutate(ID = rownames(.))




# ===========================================================================#
# combine distr. para into one dataframe
# ===========================================================================#


comb_para_gev <- inner_join(
  x = test_direct_gev_para, y = test_rf_gev_para,
  by = c("ID")
)

comb_para_gev <- inner_join(
  x = comb_para_gev, y = test_gbm_gev_para,
  by = c("ID")
)

comb_para_gev <- inner_join(
  x = comb_para_gev, y = test_svr_gev_para,
  by = c("ID")
)


# ===========================================================================#
# create grouping based on ID
# ===========================================================================#

comb_para_gev$ID = as.character(comb_para_gev$ID )

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
    ratio_loc_rf = loc_rf /loc,
    ratio_scale_rf = scale_rf / scale,
    rc_shape_rf = shape_rf-shape / abs(shape),
    ratio_loc_svr = loc_svr /loc,
    ratio_scale_svr = scale_svr / scale,
    rc_shape_svr = shape_svr-shape / abs(shape),
    ratio_loc_gbm = loc_gbm /loc,
    ratio_scale_gbm = scale_gbm / scale,
    rc_shape_gbm = shape_gbm-shape / abs(shape),
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



loc_scale_data = data_long %>%
  dplyr::filter(!(Measure == "shape"))


#data_long$method <- as.factor(data_long$method)
#levels(data_long$method)


loc_scale_data_adjusted = loc_scale_data %>%
  mutate(condition = "Adjusted")


shape_data =  data_long %>%
  dplyr::filter((Measure == "shape"))


shape_data_adjusted = shape_data %>%
  mutate(condition = "Adjusted")

save(loc_scale_data_adjusted, file = "data-raw/RObject/loc_scale_data_adjusted.RData")
save(shape_data_adjusted, file = "data-raw/RObject/shape_data_adjusted.RData")



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
  )+ylab("Relative change")
