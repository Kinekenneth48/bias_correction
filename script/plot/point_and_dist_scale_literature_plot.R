#############################################################################
# This script is created to visualize point_scale and distr. scale bias for
# Grassy Lake weather station in Wyoming.
##############################################################################

##############################################################################
# STEP 0: initial set up
##############################################################################
# =============================================================================#
# load libraries, data and functions
# =============================================================================#
library(tidyverse)
library(renv)

load("data-raw/RObject/ecdf_eg.RData")

ecdf_eg = ecdf_eg %>%
  dplyr::select(ID, NAME, STATE, maxv_WESD, pred_load)


# =============================================================================#
# convert from mm to kPa
# =============================================================================#
ecdf_eg <- ecdf_eg %>%
  mutate(
    maxv_WESD = maxv_WESD * 0.133322,
    pred_load = pred_load * 0.133322
  )

#ecdf_eg = ecdf_eg %>%
 # filter(ID == "06K08:CO:SNOW")


# Convert dataframe of data into list by ID
df_test <- ecdf_eg %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(ecdf_eg$ID))



df_ecdf = df_test[["10E08:WY:SNOW"]]

qqplot(df_ecdf$maxv_WESD, df_ecdf$pred_load, frame = FALSE)
qqline(df_ecdf$pred_load, col = "black", lwd = 2)

# =============================================================================#
# compute ecdf values
# =============================================================================#
# Compute ECDFs
ecdf_obs <- ecdf(df_ecdf$maxv_WESD)
ecdf_est <- ecdf(df_ecdf$pred_load)

# Create a sequence from the minimum to the maximum value of both columns
seq_min_max <- seq(min(c((df_ecdf$maxv_WESD), df_ecdf$pred_load)), 
                   max(c((df_ecdf$maxv_WESD), df_ecdf$pred_load)),
                   length.out = 100)

# Compute the ECDF values for the sequence
ecdf_obs_values <- ecdf_obs(seq_min_max)
ecdf_est_values <- ecdf_est(seq_min_max)

# Create data frame for plotting
df_plot <- data.frame(
  x = seq_min_max,
  ECDF_X = ecdf_obs_values,
  ECDF_Y = ecdf_est_values
)



##############################################################################
# STEP 1: create plots
##############################################################################

# Create a scatter plot with a 45 degree line
ggplot(df_ecdf, aes(x = maxv_WESD, y = pred_load)) +
  geom_point(size = 5, color = "red") +
  geom_abline(aes(colour = "one-to-one", intercept = 0, slope = 1), alpha = 1, 
              linewidth = 3, colour = "black") +
  labs(x = "Actual load", y = "Estimate load") +
  scale_x_continuous(breaks = c(seq(25, 150, 25), 175), limits = c(25, 175)) +
  scale_y_continuous(breaks = c(seq(25, 150, 25), 175), limits = c(25, 175)) +
  guides(colour = guide_legend(title = NULL)) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = c(0.83, 0.13),
    legend.direction = "vertical",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 45),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 45),
    strip.text = element_text(size = 45)
  )

# labs(x = "Ground snow load (kPa)", y = "Snow load estimate (kPa)") +

# Plot ECDFs

ggplot(df_plot, aes(x)) +
  geom_line(aes(y = ECDF_X, colour = "Actual"), size = 3) +
  geom_line(aes(y = ECDF_Y, colour = "Estimate"), size = 3) +
  labs(x = "Snow load", y = "ECDF", colour = "Quantity") +
  scale_colour_manual(values = c("Actual" = "#bf812d", "Estimate" = "#35978f")) +
  scale_x_continuous(breaks = c(seq(25, 150, 25), 175), limits = c(25, 175)) +
  guides(colour = guide_legend(title = NULL)) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = c(0.78, 0.15),
    legend.direction = "vertical",
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 45),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 45),
    strip.text = element_text(size = 45),
    legend.box.background = element_rect(fill = "grey")
  )

