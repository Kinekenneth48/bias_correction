#############################################################################
# This script is created to visualize the number of unique station count per the
# different weather station cohorts
##############################################################################
# ===========================================================================#
# load libraries
# ===========================================================================#
library(tidyverse)
library(renv)

# ===========================================================================#
# load data and preprocess
# ===========================================================================#
df <- read.csv("data-raw/data.csv")




df <- df %>%
  mutate(Network = case_when(
    base::grepl("^NY", ID) ~ "NY",
    base::grepl("^USW|^USC", ID) ~ "FOS",
    base::grepl("^USS", ID) ~ "SNOTEL",
    base::grepl("^Maine", ID) ~ "ME",
    base::substr(x = ID, start = nchar(ID) - 3, stop = nchar(ID)) == "SNOW" ~ "SC",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(Network))

# Group by ID and count the occurrences
df_unique_counts <- df %>%
  group_by(Network, YEAR) %>%
  count() %>%
  arrange(Network, YEAR)


ggplot(df_unique_counts, aes(YEAR, n)) +
  geom_line() +
  facet_wrap(~Network, scales = "free", ncol = 2) +
  # ylim(c(0, 800))+
  ylab("Unique station count") +
  xlab("Year") +
  theme(
    legend.position = c(0.85, 0.1),
    legend.direction = "vertical",
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 35),
    axis.text = element_text(size = 30),
    strip.text = element_text(size = 30)
  )
