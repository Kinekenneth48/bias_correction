
#############################################################################
###### STEP 0: Initial Setup
#############################################################################
# load libraries
library(tidyverse)
library(renv)
library(ranger)
library(sf)
library(raster)
library(lubridate)

# load data
load("data-raw/RObject/rf_main_coop_fos_sntl.RData")
load("data-raw/RObject/df_snotel_full.RData")



source(file = "R/dsc.R")
source(file = "R/ecoregion.R")

# load meta data of all GHCND stations
stations <- read_csv("data-raw/stations.csv", show_col_types = FALSE)


#############################################################################
###### STEP 1: Impute SWE in SNOTEL DATA
#############################################################################

# =============================================================================#
# Step 11: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_snotel <- dplyr::left_join(
  x = df_snotel_full,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step 12: add climate data
# =============================================================================#
load("data-raw/RObject/climate_data_sntl.Rdata")


climate_data_sntl <- climate_data_sntl %>%
  dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
  dplyr::rename(
    DATE = date, SRAD = srad, WIND_VEL = wind_vel,
    VPD = vpd, TMEAN = tmean
  )


df_snotel <- dplyr::left_join(
  x = df_snotel, y = climate_data_sntl,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 13: update data with ecoregion
# =============================================================================#

# load shape file of ecoregions
ecoregions_all <-
  st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_snotel <- ecoregion(ecoregions_all, df_snotel)



# =============================================================================#
# Step 14: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
shoreline <- st_read(
  dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
  layer = "GSHHS_c_L1"
)

shoreline2 <- st_read(
  dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
  layer = "GSHHS_c_L2"
)


df_snotel <- dsc(shoreline, shoreline2, df_snotel)



# =============================================================================#
# Step 15: update data with rolling avrages and number of days snow stayed on ground
# =============================================================================#

df_snotel <- df_snotel %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 16: impute SWE
# =============================================================================#

#filter(ID == "USS0011E35S")


df_impute_no_impute <- df_snotel %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))

df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNWD, WESD
  )


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN), TMEAN, ROLL_AVG_TMEAN),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD, ROLL_AVG_SRAD),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL), WIND_VEL,
      ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNWD, WESD, WESD_I
  )

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_sntl_with_imputed_wesd <- rbind(df_impute, df_no_impute)


df_sntl_with_imputed_wesd$MONTH <- as.numeric(format(
  df_sntl_with_imputed_wesd$DATE,
  "%m"
))

# remove imputation for non snow months
df_sntl_with_imputed_wesd <- df_sntl_with_imputed_wesd %>%
  dplyr::mutate(d1 = ifelse(MONTH %in% c(6, 7, 8, 9, 10), 1, 0)) %>%
  dplyr::mutate(WESD = ifelse(d1 == 1 & SNWD > 0, NA, WESD))



save(df_sntl_with_imputed_wesd,
  file = "data-raw/RObject/df_sntl_with_imputed_wesd.RData"
)

save(df_sntl_with_imputed_wesd,
  file = "data-raw/RObject/df_sntl_with_imputed_wesd_nosnwd_impute.RData"
)



rm(df_sntl_with_imputed_wesd)
rm(climate_data_sntl)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)
rm(df_snotel_full)
rm(df_snotel)




#############################################################################
###### STEP 2: Impute SWE in FOS AND COOP DATA
#############################################################################

load("data-raw/RObject/df_full_fos_coop_main.RData") # fos and coop with imputed snwd

#############################################################################
#### STEP 21:Split data due to memory issues to impute SWE
#############################################################################

t <- unique(df_full_fos_coop_main$ID)

id1 <- t[1:500]
id2 <- t[501:1000]
id3 <- t[1001:1500]
id4 <- t[1501:2000]
id5 <- t[2001:2500]
id6 <- t[2501:3000]
id7 <- t[3001:3500]
id8 <- t[3501:4000]
id9 <- t[4001:4500]
id10 <- t[4501:5000]
id11 <- t[5001:5500]
id12 <- t[5501:length(t)]


##########################################################################
df_full_fos_coop_1 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id1)

save(df_full_fos_coop_1,
  file = "data-raw/RObject/df_full_fos_coop_1_nosnwd_impute.RData"
)
rm(df_full_fos_coop_1)
rm(id1)

##########################################################################
df_full_fos_coop_2 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id2)

save(df_full_fos_coop_2,
  file = "data-raw/RObject/df_full_fos_coop_2_nosnwd_impute.RData"
)
rm(df_full_fos_coop_2)
rm(id2)
##########################################################################

df_full_fos_coop_3 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id3)

save(df_full_fos_coop_3,
  file = "data-raw/RObject/df_full_fos_coop_3_nosnwd_impute.RData"
)
rm(df_full_fos_coop_3)
rm(id3)
##########################################################################
df_full_fos_coop_4 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id4)

save(df_full_fos_coop_4,
  file = "data-raw/RObject/df_full_fos_coop_4_nosnwd_impute.RData"
)

rm(df_full_fos_coop_4)
rm(id4)
##########################################################################
df_full_fos_coop_5 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id5)

save(df_full_fos_coop_5,
  file = "data-raw/RObject/df_full_fos_coop_5_nosnwd_impute.RData"
)
rm(df_full_fos_coop_5)
rm(id5)
##########################################################################
df_full_fos_coop_6 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id6)

save(df_full_fos_coop_6,
  file = "data-raw/RObject/df_full_fos_coop_6_nosnwd_impute.RData"
)
rm(df_full_fos_coop_6)
rm(id6)
##########################################################################
df_full_fos_coop_7 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id7)


save(df_full_fos_coop_7,
  file = "data-raw/RObject/df_full_fos_coop_7_nosnwd_impute.RData"
)
rm(df_full_fos_coop_7)
rm(id7)
##########################################################################
df_full_fos_coop_8 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id8)

save(df_full_fos_coop_8,
  file = "data-raw/RObject/df_full_fos_coop_8_nosnwd_impute.RData"
)

rm(df_full_fos_coop_8)
rm(id8)
##########################################################################
df_full_fos_coop_9 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id9)

save(df_full_fos_coop_9,
  file = "data-raw/RObject/df_full_fos_coop_9_nosnwd_impute.RData"
)

rm(df_full_fos_coop_9)
rm(id9)
##########################################################################
df_full_fos_coop_10 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id10)

save(df_full_fos_coop_10,
  file = "data-raw/RObject/df_full_fos_coop_10_nosnwd_impute.RData"
)
rm(df_full_fos_coop_10)
rm(id10)
##########################################################################
df_full_fos_coop_11 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id11)

save(df_full_fos_coop_11,
  file = "data-raw/RObject/df_full_fos_coop_11_nosnwd_impute.RData"
)

rm(df_full_fos_coop_11)
rm(id11)
##########################################################################
df_full_fos_coop_12 <- df_full_fos_coop_main %>%
  dplyr::filter(ID %in% id12)

save(df_full_fos_coop_12,
  file = "data-raw/RObject/df_full_fos_coop_12_nosnwd_impute.RData"
)
rm(df_full_fos_coop_12)
rm(id12)
##########################################################################



# remove unwanted data
rm(df_full_fos_coop_main)
rm(t)





#############################################################################
#### STEP 22: impute SWE
#############################################################################
#############################################################################
########## 1 
#############################################################################

load("data-raw/RObject/df_full_fos_coop_1_nosnwd_impute.RData")

load("data-raw/RObject/rf_main_coop_fos_sntl.RData")
# =============================================================================#
# Step 1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_1,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
load("data-raw/RObject/climate_data_fos_coop.RData")

climate_data_fos_coop <- climate_data_fos_coop %>%
  dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
  dplyr::rename(
    DATE = date, SRAD = srad, WIND_VEL = wind_vel,
    VPD = vpd, TMEAN = tmean
  )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
ecoregions_all <-
  st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
shoreline <- st_read(
  dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
  layer = "GSHHS_c_L1"
)

shoreline2 <- st_read(
  dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
  layer = "GSHHS_c_L2"
)


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#



df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))

df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

# df_fos_coop_with_imputed_wesd1= df_no_impute
df_fos_coop_with_imputed_wesd1_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()

save(df_fos_coop_with_imputed_wesd1_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd1_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd1_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_1)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)





#############################################################################
########## 2
#############################################################################
load("data-raw/RObject/df_full_fos_coop_2_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_2,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd2_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()

save(df_fos_coop_with_imputed_wesd2_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd2_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd2_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_2)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)






#############################################################################
########## 3
#############################################################################

load("data-raw/RObject/df_full_fos_coop_3_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_3,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd3_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()

save(df_fos_coop_with_imputed_wesd3_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd3_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd3_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_3)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)






#############################################################################
########## 4
#############################################################################

load("data-raw/RObject/df_full_fos_coop_4_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_4,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # # load shape file of ecoregions
#  ecoregions_all <-
#    st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)


df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

# df_fos_coop_with_imputed_wesd4 <- df_impute_no_impute
df_fos_coop_with_imputed_wesd4_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd4_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd4_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd4_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_4)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)









#############################################################################
########## 5
#############################################################################

load("data-raw/RObject/df_full_fos_coop_5_nosnwd_impute.RData")
load("data-raw/RObject/rf_main_coop_fos_sntl.RData")
# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_5,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd5_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd5_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd5_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd5_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_5)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)









#############################################################################
########## 6
#############################################################################

load("data-raw/RObject/df_full_fos_coop_6_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_6,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd6_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd6_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd6_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd6_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_6)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)









#############################################################################
########## 7
#############################################################################
load("data-raw/RObject/df_full_fos_coop_7_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_7,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd7_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd7_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd7_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd7_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_7)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)









#############################################################################
#########8
#############################################################################

load("data-raw/RObject/df_full_fos_coop_8_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_8,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA

df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)


df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd8_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd8_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd8_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd8_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_8)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)










#############################################################################
########## 9
#############################################################################

load("data-raw/RObject/df_full_fos_coop_9_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_9,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd9_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd9_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd9_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd9_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_9)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)










#############################################################################
##########10
#############################################################################

load("data-raw/RObject/df_full_fos_coop_10_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_10,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd10_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd10_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd10_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd10_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_10)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)











#############################################################################
########## 11
#############################################################################

load("data-raw/RObject/df_full_fos_coop_11_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_11,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA


df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)


df_fos_coop_with_imputed_wesd11_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd11_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd11_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd11_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_11)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)











#############################################################################
########## 12
#############################################################################

load("data-raw/RObject/df_full_fos_coop_12_nosnwd_impute.RData")

# =============================================================================#
# Step1: add station meta data and compute DAYS away from start of snow year
# =============================================================================#

stations <- stations %>%
  dplyr::select(ID, NAME, STATE, LONGITUDE, LATITUDE, ELEVATION)

df_fos_coop <- dplyr::left_join(
  x = df_full_fos_coop_12,
  y = stations, by = c("ID")
) %>%
  dplyr::mutate(
    YEAR = lubridate::year(DATE),
    MONTH = lubridate::month(DATE)
  ) %>%
  dplyr::mutate(YEAR = dplyr::if_else(MONTH > 6, YEAR + 1, YEAR)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(START_DATE = ymd(paste((YEAR - 1), "11", "01", sep = "-"))) %>%
  dplyr::mutate(DAYS = as.double(difftime(DATE, START_DATE,
    units = c("days")
  ))) %>%
  dplyr::select(
    ID, NAME, STATE, LONGITUDE, LATITUDE,
    ELEVATION, DATE, YEAR, SNOW, SNWD, WESD, DAYS
  ) %>%
  ungroup()



# =============================================================================#
# Step1: add climate data
# =============================================================================#
# climate_data_fos_coop <- climate_data_fos_coop %>%
#   dplyr::select(ID, date, srad, wind_vel, vpd, tmean) %>%
#   dplyr::rename(
#     DATE = date, SRAD = srad, WIND_VEL = wind_vel,
#     VPD = vpd, TMEAN = tmean
#   )


df_fos_coop <- dplyr::left_join(
  x = df_fos_coop, y = climate_data_fos_coop,
  by = c("ID", "DATE")
)


# =============================================================================#
# Step 2: update data with ecoregion
# =============================================================================#

# # load shape file of ecoregions
# ecoregions_all <-
#   st_read("data-raw/eco_regions/eco_regions/NA_CEC_Eco_Level3.shp")


df_fos_coop <- ecoregion(ecoregions_all, df_fos_coop)



# =============================================================================#
# Step 3: update data with distance to coast or major lake
# =============================================================================#
# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
# shoreline <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L1"
# )
#
# shoreline2 <- rgdal::readOGR(
#   dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
#   layer = "GSHHS_c_L2"
# )


df_fos_coop <- dsc(shoreline, shoreline2, df_fos_coop)



# =============================================================================#
# Step 4: update data with rolling averages and number of days snow stayed on ground
# =============================================================================#

df_fos_coop <- df_fos_coop %>%
  dplyr::arrange(ID, DATE) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(DATE) > 1))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID, grp) %>%
  dplyr::mutate(rec = 1) %>%
  dplyr::mutate(
    ROLL_AVG_VPD = cumsum(VPD) / cumsum(rec),
    ROLL_AVG_TMEAN = cumsum(TMEAN) / cumsum(rec),
    ROLL_AVG_SRAD = cumsum(SRAD) / cumsum(rec),
    ROLL_AVG_WIND_VEL = cumsum(WIND_VEL) / cumsum(rec)
  ) %>%
  dplyr::group_modify(~ {
    .x %>%
      dplyr::mutate(GROUND = row_number())
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rec, grp))


# =============================================================================#
# Step 5: impute SWE
# =============================================================================#


df_impute_no_impute <- df_fos_coop %>%
  dplyr::mutate(D1 = ifelse((!is.na(TMEAN) & !is.na(SRAD) & !is.na(VPD) &
    !is.na(WIND_VEL) &
    !is.na(SNWD)  & !(SNWD == 0)),
  "impute", "no_impute"
  )) %>%
  dplyr::mutate(WESD = ifelse(SNWD == 0 & is.na(WESD), 0, WESD))



df_no_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "no_impute") %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD)


df_to_impute <- df_impute_no_impute %>%
  dplyr::filter(D1 == "impute") %>%
  dplyr::mutate(
    ROLL_AVG_VPD = ifelse(is.na(ROLL_AVG_VPD), VPD, ROLL_AVG_VPD),
    ROLL_AVG_TMEAN = ifelse(is.na(ROLL_AVG_TMEAN),
      TMEAN, ROLL_AVG_TMEAN
    ),
    ROLL_AVG_SRAD = ifelse(is.na(ROLL_AVG_SRAD), SRAD,
      ROLL_AVG_SRAD
    ),
    ROLL_AVG_WIND_VEL = ifelse(is.na(ROLL_AVG_WIND_VEL),
      WIND_VEL, ROLL_AVG_WIND_VEL
    )
  )
# NA CREATED BEFORE 1979 CLIMATE DATA

df_to_impute$ratio_p <- predict(rf_main_coop_fos_sntl, df_to_impute)$predictions


df_impute <- df_to_impute %>%
  dplyr::mutate(WESD_I = ratio_p * SNWD) %>%
  dplyr::select(ID, DATE, SNOW, SNWD, WESD, WESD_I)

df_no_impute <- df_no_impute %>%
  mutate(WESD_I = NA)

df_fos_coop_with_imputed_wesd12_nosnwd_impute <- rbind(df_impute, df_no_impute)
gc()
save(df_fos_coop_with_imputed_wesd12_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd12_nosnwd_impute.RData"
)


rm(df_fos_coop_with_imputed_wesd12_nosnwd_impute)
rm(df_fos_coop)
rm(df_full_fos_coop_12)
rm(df_impute)
rm(df_impute_no_impute)
rm(df_no_impute)
rm(df_to_impute)
rm(rf_main_coop_fos_sntl)


############################################################################
# STEP 3: Combine the multiple data into two sections
############################################################################


load("data-raw/RObject/df_fos_coop_with_imputed_wesd1_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd2_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd3_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd4_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd5_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd6_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd7_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd8_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd9_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd10_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd11_nosnwd_impute.RData")
load("data-raw/RObject/df_fos_coop_with_imputed_wesd12_nosnwd_impute.RData")

####################################################################################

df_fos_coop_with_imputed_wesd_part1_nosnwd_impute <- rbind(
  df_fos_coop_with_imputed_wesd1_nosnwd_impute,
  df_fos_coop_with_imputed_wesd2_nosnwd_impute,
  df_fos_coop_with_imputed_wesd3_nosnwd_impute,
  df_fos_coop_with_imputed_wesd4_nosnwd_impute,
  df_fos_coop_with_imputed_wesd5_nosnwd_impute,
  df_fos_coop_with_imputed_wesd6_nosnwd_impute,
  df_fos_coop_with_imputed_wesd7_nosnwd_impute
)



df_fos_coop_with_imputed_wesd_part1_nosnwd_impute$MONTH <-
  as.numeric(format(
    df_fos_coop_with_imputed_wesd_part1_nosnwd_impute$DATE,
    "%m"
  ))

# remove imputation for non snow months
df_fos_coop_with_imputed_wesd_part1_nosnwd_impute <-
  df_fos_coop_with_imputed_wesd_part1_nosnwd_impute %>%
  dplyr::mutate(d1 = ifelse(MONTH %in% c(6, 7, 8, 9, 10), 1, 0)) %>%
  dplyr::mutate(WESD = ifelse(d1 == 1 & SNWD > 0, NA, WESD)) %>%
  dplyr::select(-c(d1))



save(df_fos_coop_with_imputed_wesd_part1_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd_part1_nosnwd_impute.RData"
)

remove(df_fos_coop_with_imputed_wesd_part1_nosnwd_impute)
remove(
  df_fos_coop_with_imputed_wesd1_nosnwd_impute,
  df_fos_coop_with_imputed_wesd2_nosnwd_impute,
  df_fos_coop_with_imputed_wesd3_nosnwd_impute,
  df_fos_coop_with_imputed_wesd4_nosnwd_impute,
  df_fos_coop_with_imputed_wesd5_nosnwd_impute,
  df_fos_coop_with_imputed_wesd6_nosnwd_impute,
  df_fos_coop_with_imputed_wesd7_nosnwd_impute
)


#############################################################################
df_fos_coop_with_imputed_wesd_part2_nosnwd_impute <- rbind(
  df_fos_coop_with_imputed_wesd8_nosnwd_impute,
  df_fos_coop_with_imputed_wesd9_nosnwd_impute,
  df_fos_coop_with_imputed_wesd10_nosnwd_impute,
  df_fos_coop_with_imputed_wesd11_nosnwd_impute,
  df_fos_coop_with_imputed_wesd12_nosnwd_impute
)


df_fos_coop_with_imputed_wesd_part2_nosnwd_impute$MONTH <-
  as.numeric(format(
    df_fos_coop_with_imputed_wesd_part2_nosnwd_impute$DATE,
    "%m"
  ))

# remove imputation for non snow months
df_fos_coop_with_imputed_wesd_part2_nosnwd_impute <-
  df_fos_coop_with_imputed_wesd_part2_nosnwd_impute %>%
  dplyr::mutate(d1 = ifelse(MONTH %in% c(6, 7, 8, 9, 10), 1, 0)) %>%
  dplyr::mutate(WESD = ifelse(d1 == 1 & SNWD > 0, NA, WESD)) %>%
  dplyr::select(-c(d1))


save(df_fos_coop_with_imputed_wesd_part2_nosnwd_impute,
  file = "data-raw/RObject/df_fos_coop_with_imputed_wesd_part2_nosnwd_impute.RData"
)


remove(
  df_fos_coop_with_imputed_wesd8_nosnwd_impute,
  df_fos_coop_with_imputed_wesd9_nosnwd_impute,
  df_fos_coop_with_imputed_wesd10_nosnwd_impute,
  df_fos_coop_with_imputed_wesd11_nosnwd_impute,
  df_fos_coop_with_imputed_wesd12_nosnwd_impute
)
remove(df_fos_coop_with_imputed_wesd_part2_nosnwd_impute)
