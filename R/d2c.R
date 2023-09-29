
# =============================================================================

# Step 1: Prepare data
# =============================================================================
#load libraries
library(units)
library(sf)
library(tidyverse)
library(tigris)
library(units)


# Extract location coordinates  for stations
sites <- unique(GHCND_all_new[c("ID", "NAME", "LONGITUDE", "LATITUDE")]) %>%
 dplyr::filter(!is.na(LONGITUDE)) %>%
  as.data.frame(.)

# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
shoreline <- rgdal::readOGR(
  dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
  layer = "GSHHS_c_L1"
)

shoreline2 <- rgdal::readOGR(
  dsn = "data-raw/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/c",
  layer = "GSHHS_c_L2"
)


# Crop according to approximate coordinate extent of the United States
# (obtained via Google Earth). (minLon, maxLon, minLat, maxLat)
shoreline_crop <- raster::crop(
  shoreline,
  raster::extent(c(-180, -60, 25, 71.5))
)
shoreline2_crop <- raster::crop(
  shoreline2,
  raster::extent(c(-180, -60, 25, 71.5))
)


# Only keeps the largest lakes (i.e. the great lakes)
shoreline2_crop <- shoreline2_crop[shoreline2_crop$area > 20000, ]

# Only keep the North American continent (which has the second largest area,
# with Eurasia having the largest area)
shoreline_crop <- shoreline_crop[shoreline_crop$area > 1e07 &
  shoreline_crop$area < 4e07, ]

# Confirm that the final shapefiles look as expected.
sp::plot(shoreline_crop)
sp::plot(shoreline2_crop, col = "blue", add = TRUE)



# =============================================================================

# Step 2: Obtain distance to coast calculations.
# =============================================================================
# set sites dataframe as sf object
sites_geo <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE")) %>%
  sf::st_set_crs(., 4326)

# 2163 = US National Atlas


# set shoreline data as sf object
shoreline_crop = sf::st_as_sf(shoreline_crop) 
#shoreline_crop <- sf::st_transform(shoreline_crop, sf::st_crs(sites_geo))

shoreline2_crop = sf::st_as_sf(shoreline2_crop) 
#shoreline2_crop <- sf::st_transform(shoreline2_crop, sf::st_crs(sites_geo))



#transform shoreline_crop(coastline) from polygon shape to line
shoreline_crop = sf::st_cast(shoreline_crop, "MULTILINESTRING")
shoreline2_crop = sf::st_cast(shoreline2_crop, "MULTILINESTRING")

# Confirm that the final shape files look as expected.
plot(shoreline_crop, max.plot =1)
plot(shoreline2_crop, max.plot =1)


#calculate distance between coasts and the snow stations
d1 = sf::st_distance( shoreline_crop, sites_geo)
d1 =units::set_units(d1, "km")
colnames(d1 ) = sites_geo$ID
rownames(d1 ) = shoreline_crop$id
 d2c=  data.frame(sites_geo$ID,d1[1,]) %>%
   units::drop_units(.)
colnames(d2c) = c("ID", "D2C_km")



#calculate distance between major lakes and the snow stations
d2 = sf::st_distance( shoreline2_crop, sites_geo)
d2 = units::set_units(d2, "km")
colnames(d2 ) = sites_geo$ID
rownames(d2 ) = shoreline2_crop$id
d2l = data.frame(d2[,]) %>%
  t(.)

d2l=  data.frame(sites_geo$ID,d2l)
names(d2l)[1] = "ID"


comb_dist = merge(d2c, d2l, by="ID")

# If a "great lake" or "Alaskan Island" shoreline is closer, use
# that in favor of the continental US shoreline.
#comb_dist$min <- do.call(pmin, comb_dist)
comb_dist$min <- apply(comb_dist[,2:7], 1, FUN =min)



comb_dist = comb_dist %>% dplyr::select(ID, min)

other_stations_d2c_ecoregion <- read_excel("data-raw/other_stations_d2c_ecoregion.xlsx")

other_stations_d2c_ecoregion = other_stations_d2c_ecoregion %>%
  dplyr::select(ID, D2C) %>%
  dplyr::rename( min= D2C)

#add D2C for other 15 stations
comb_dist = bind_rows(comb_dist, other_stations_d2c_ecoregion)

# Save an object to a Rdata
save(comb_dist, file = "data-raw/RObjects/comb_dist.Rdata")

# Retain only the shortest distance for each location.
complete_ghcnd_tmean_ecoregion_d2c = merge(complete_ghcnd_tmean_ecoregion, 
                                           comb_dist, by ="ID") %>%dplyr:: rename(D2C =min)
# Save an object to a Rdata
save(complete_ghcnd_tmean_ecoregion_d2c, 
     file = "data-raw/RObjects/complete_ghcnd_tmean_ecoregion_d2c.Rdata")

