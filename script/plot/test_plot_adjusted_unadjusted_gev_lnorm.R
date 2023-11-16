#############################################################################
# This script is created to visualize the log normal and GEV distr parameters when
# bias is corrected OR NOT for the test data.
##############################################################################

##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries and data
# =============================================================================#
library(tidyverse)
library(renv)

base::load("data-raw/Robject/data_long_adjusted.RData")
base::load("data-raw/Robject/data_long_unadjusted.RData")
base::load("data-raw/RObject/loc_scale_data_unadjusted.RData")
base::load("data-raw/RObject/loc_scale_data_adjusted.RData")
base::load("data-raw/RObject/shape_data_unadjusted.RData")
base::load("data-raw/RObject/shape_data_adjusted.RData")


##############################################################################
# STEP 1: lnorm
##############################################################################
six_grid <- rbind(data_long_adjusted, data_long_unadjusted)

one_grid <- data_long_unadjusted %>%
  filter(method == "RF")

# GET SAMPLE SIZE OF NETWORKS
size <- one_grid %>%
  dplyr::filter(Measure == "mean") %>%
  group_by(Network) %>%
  summarise(count = n())



# Set the order of levels for the 'condition' factor
six_grid$condition <- factor(six_grid$condition, levels = c("Unadjusted", "Adjusted"))


ggplot(six_grid, aes(x = Network, y = Ratio, fill = Measure)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_grid(method ~ condition) +
  ylim(c(0.6, 1.4)) +
  scale_fill_manual(
    values = c("mean" = "#7570b3", "sd" = "#d95f02"),
    labels = c("mean" = expression(mu), "sd" = expression(sigma))
  ) +
  ggtitle("Boxplot ") +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    axis.title.x = element_blank(), # Remove x-axis title
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text.x = element_text(size = 40, margin = margin(t = 10, b = 10)),
    axis.text.y = element_text(size = 40, margin = margin(r = 10, l = 10)),
    strip.text = element_text(size = 40),
    panel.spacing = grid::unit(1, "lines")
  )


ggplot(one_grid, aes(x = Network, y = Ratio, fill = Measure)) +
  geom_boxplot(width = 0.5) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  ylim(c(0.6, 1.4)) +
  scale_fill_manual(
    values = c("mean" = "#7570b3", "sd" = "#d95f02"),
    labels = c("mean" = expression(mu), "sd" = expression(sigma))
  ) +
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



##############################################################################
# STEP 2: gev
##############################################################################

six_grid_gev <- rbind(loc_scale_data_unadjusted, loc_scale_data_adjusted)

# Set the order of levels for the 'condition' factor
six_grid_gev$condition <- factor(six_grid_gev$condition, levels = c("Unadjusted", "Adjusted"))



ggplot(six_grid_gev, aes(x = Network, y = Ratio, fill = Measure)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  facet_grid(method ~ condition) +
  ylim(c(0.6, 1.4)) +
  scale_fill_manual(
    values = c("location" = "#ffff99", "scale" = "#b15928"),
    labels = c("location" = expression(mu), "scale" = expression(sigma))
  ) +
  ggtitle("Boxplot ") +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    axis.title.x = element_blank(), # Remove x-axis title
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text.x = element_text(size = 40, margin = margin(t = 10, b = 10)),
    axis.text.y = element_text(size = 40, margin = margin(r = 10, l = 10)),
    strip.text = element_text(size = 40),
    panel.spacing = grid::unit(1, "lines")
  )




##############################################################################
# STEP 3: gev - shape
##############################################################################

six_grid_gev_shape <- rbind(shape_data_adjusted, shape_data_unadjusted)

six_grid_gev_shape <- six_grid_gev_shape %>%
  mutate(Ratio = Ratio * 100)

# Set the order of levels for the 'condition' factor
six_grid_gev_shape$condition <- factor(six_grid_gev_shape$condition, levels = c("Unadjusted", "Adjusted"))

six_grid_gev_shape <- six_grid_gev_shape %>%
  select(Ratio, method, condition, Network)


ggplot(six_grid_gev_shape, aes(x = Network, y = Ratio)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(method ~ condition) +
  ylim(c(-150, 150)) +
  ggtitle("Boxplot ") +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 35),
    strip.text = element_text(size = 35),
    panel.spacing = grid::unit(1, "lines")
  ) +
  ylab(expression("Relative change of" ~ xi ~ "(in %)"))
