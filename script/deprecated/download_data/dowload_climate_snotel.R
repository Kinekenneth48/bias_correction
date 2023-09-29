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
load("data-raw/RObject/df_snotel_full.RData")


# load meta data of all GHCND stations
stations <- read_csv("data-raw/stations.csv", show_col_types = FALSE)

df_snotel_full %>% filter(ID == "USS0011G05S")

# =============================================================================#
# Step 1: Download climate data from GridMet for SNOTEL station
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)


# join data frames
combined <- dplyr::left_join(
  x = df_snotel_full, y = stations, by = c("ID")
) %>%
  dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  dplyr::filter(DATE >= "1979/01/01") %>%
  dplyr::filter(between(LONGITUDE, -130, -65)) %>%
  dplyr::filter(between(LATITUDE, 25, 50))




# get AOI for site of interest
sites <- combined %>%
  dplyr::ungroup() %>%
  dplyr::select(ID, NAME, STATE, DATE, LONGITUDE, LATITUDE) %>%
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

## Run the analysis in parallel on the local computer
future::plan("multisession", workers = 14)

## Regardless of the future plan, the number of workers, and
## where they are, the random numbers produced are identical
set.seed(14425)


empty_list_sntl <- future_lapply(sites_sf,
  FUN = get_climate_station,
  future.packages = c("climateR", "tidyverse")
)


## Shut down parallel workers
future::plan("sequential")



# create empty list of hold individual climate data
empty_list_sntl <- vector(mode = "list", length = length(sites_sf))

tictoc::tic()
for (i in 1:length(sites_sf)) {
  empty_list_sntl[[i]] <- try(
    climateR::getGridMET(
      AOI = sites_sf[[i]],
      param = c("tmin", "tmax", "vpd", "wind_vel", "srad"),
      startDate = sites_sf[[i]]$startDate,
      endDate = sites_sf[[i]]$endDate
    ) %>%
      dplyr::mutate(ID = sites_sf[[i]]$ID)
  )
  # close connections
  closeAllConnections()
}
tictoc::toc()
# 2 hours

# remove data frames in list with error

empty_list_sntl <- empty_list_sntl[sapply(
  empty_list_sntl,
  function(x) !inherits(x, "try-error")
)]



# row bind all climate data in list
climate_data_sntl <- bind_rows(empty_list_sntl)

climate_data_sntl <- climate_data_sntl %>%
  dplyr::mutate(tmean = (tmin + tmax) / 2) %>%
  dplyr::select(-c("tmin", "tmax"))

# Saving on object in RData format
save(climate_data_sntl, file = "data-raw/RObject/climate_data_sntl.Rdata")
