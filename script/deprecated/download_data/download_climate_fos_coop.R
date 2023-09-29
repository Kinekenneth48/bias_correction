# =============================================================================#
# Step 0: set-up
# =============================================================================#
library(lubridate)
library(tidyverse)
library(sf)
library(AOI)
library(climateR)
library(tictoc)

source(file = "R/convert_sf.R")
source(file = "R/get_climate.R")

# load station data
load("data-raw/RObject/df_full_fos_coop_main.RData")

# load meta data of all GHCND stations
stations <- read_csv("data-raw/stations.csv", show_col_types = FALSE)




# =============================================================================#
# Step 1: Download climate data from GridMet for FOS/COOP station
# =============================================================================#
stations <- stations %>%
  dplyr::select(ID, LONGITUDE, LATITUDE)

df_full_fos_coop_main <- df_full_fos_coop_main %>%
  dplyr::select(ID, DATE)

# join data frames
combined <- dplyr::left_join(
  x = df_full_fos_coop_main, y = stations, by = c("ID")
) %>%
  dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  dplyr::filter(DATE >= "1979/01/01") %>%
  dplyr::filter(between(LONGITUDE, -130, -65)) %>%
  dplyr::filter(between(LATITUDE, 25, 50))




# get AOI for site of interest
sites <- combined %>%
  dplyr::ungroup() %>%
  dplyr::select(ID, DATE, LONGITUDE, LATITUDE) %>%
  dplyr::group_by(ID) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(startDate = min(.x$DATE)) %>%
      dplyr::mutate(endDate = max(.x$DATE))
  }) %>%
  dplyr::distinct(ID, .keep_all = TRUE) %>%
  dplyr::group_split()



# convert df to sf  with point geometries
sites_sf <- lapply(X = sites, convert_sf)



# get climate information

# create empty list of hold individual climate data
empty_list_fos_coop <- vector(mode = "list", length = length(sites_sf))

tic()
for (i in 1:length(sites_sf)) {
  empty_list_fos_coop[[i]] <- try(
    climateR::getGridMET(
      AOI = sites_sf[[i]],
      param = c("tmin", "tmax", "vpd", "wind_vel", "srad", "prcp"),
      startDate = sites_sf[[i]]$startDate,
      endDate = sites_sf[[i]]$endDate
    ) %>%
      dplyr::mutate(ID = sites_sf[[i]]$ID)
  )
  # close connections
  closeAllConnections()
}
toc()


# remove data frames in list with error
empty_list_fos_coop <- empty_list_fos_coop[sapply(
  empty_list_fos_coop,
  function(x) !inherits(x, "try-error")
)]
# empty_list[[630]] = NULL


# row bind all climate data in list
climate_data_fos_coop <- bind_rows(empty_list_fos_coop)

climate_data_fos_coop <- climate_data_fos_coop %>%
  dplyr::mutate(tmean = (tmin + tmax) / 2) %>%
  dplyr::select(-c("tmin", "tmax"))

# Saving on object in RData format
save(climate_data_fos_coop,
     file = "data-raw/RObject/climate_data_fos_coop.Rdata"
)
