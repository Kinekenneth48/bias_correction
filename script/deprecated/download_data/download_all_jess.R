# =============================================================================#
# Step 0: set-up
# =============================================================================#
library(lubridate)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(units)
library(AOI)
library(climateR)
library(tictoc)

source(file = "R/convert_sf.R")
source(file = "R/get_climate.R")

load("data-raw/RObject/maine_complete.RData")
load("data-raw/RObject/NY_data.RData")
load("data-raw/RObject/st_paul_with_elevation.RData")
load("data-raw/RObject/SnowSurvey2020_with_elevation.RData")
load("data-raw/RObject/GHCND_all_new.RData")
load("data-raw/jess_id.RData")


# load meta data of all GHCND stations
stations <- read_csv("data-raw/stations.csv", show_col_types = FALSE)

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)


# =============================================================================#
# Step1: combine ghcnd, NY,MAINE,ST. PAUL, and Snow Survey dataset
# =============================================================================#

GHCND_all_new <- GHCND_all_new[, -11]

combined <- rbind(
  maine_complete, NY_data, st_paul_with_elevation,
  SnowSurvey2020_with_elevation, GHCND_all_new
) 