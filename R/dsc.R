# This function is used to compute the distance to the nearest coast or major water body

dsc <- function(shoreline, shoreline2, df) {

  # set relation_to_geometry attribute of an sf object to constant
  st_agr(shoreline) <- "constant"
  st_agr(shoreline2) <- "constant"
  
  # Crop according to approximate coordinate extent of the United States
  # (obtained via Google Earth). (minLon, maxLon, minLat, maxLat)
  shoreline_crop <- st_crop(
    x = shoreline,
    xmin = -180, xmax = -60, ymin = 25, ymax = 71.5
  )

  shoreline2_crop <- st_crop(
    x = shoreline2,
    xmin = -180, xmax = -60, ymin = 25, ymax = 71.5
  )


  # Only keeps the largest lakes (i.e. the great lakes)
  shoreline2_crop <- shoreline2_crop[shoreline2_crop$area > 20000, ]

  # Only keep the North American continent (which has the second largest area,
  # with Eurasia having the largest area)
  shoreline_crop <- shoreline_crop[shoreline_crop$area > 1e07 &
    shoreline_crop$area < 4e07, ]

  # Extract location coordinates  for stations
  sites <- unique(df[c("ID", "NAME", "LONGITUDE", "LATITUDE")]) %>%
    dplyr::filter(!is.na(LONGITUDE)) %>%
    as.data.frame(.)

  # set sites data frame as sf object
  sites_geo <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE")) %>%
    sf::st_set_crs(., 4326)


  # set shoreline data as sf object
  shoreline_crop <- sf::st_as_sf(shoreline_crop)
  # shoreline_crop <- sf::st_transform(shoreline_crop, sf::st_crs(sites_geo))

  shoreline2_crop <- sf::st_as_sf(shoreline2_crop)
  # shoreline2_crop <- sf::st_transform(shoreline2_crop, sf::st_crs(sites_geo))



  # transform shoreline_crop(coastline) from polygon shape to line
  shoreline_crop <- sf::st_cast(shoreline_crop, "MULTILINESTRING")
  shoreline2_crop <- sf::st_cast(shoreline2_crop, "MULTILINESTRING")



  # calculate distance between coasts and the snow stations
  d1 <- sf::st_distance(shoreline_crop, sites_geo)
  d1 <- units::set_units(d1, "km")
  colnames(d1) <- sites_geo$ID
  rownames(d1) <- shoreline_crop$id
  d2c <- data.frame(sites_geo$ID, d1[1, ]) %>%
    units::drop_units(.)
  colnames(d2c) <- c("ID", "D2C_km")


  # calculate distance between major lakes and the snow stations
  d2 <- sf::st_distance(shoreline2_crop, sites_geo)
  d2 <- units::set_units(d2, "km")
  colnames(d2) <- sites_geo$ID
  rownames(d2) <- shoreline2_crop$id
  d2l <- data.frame(d2[, ]) %>%
    t(.)

  d2l <- data.frame(sites_geo$ID, d2l)
  names(d2l)[1] <- "ID"


  comb_dist <- merge(d2c, d2l, by = "ID")
  comb_dist <- comb_dist[!duplicated(comb_dist[, c("ID")]), ]

  # If a "great lake" or "Alaskan Island" shoreline is closer, use
  # that in favor of the continental US shoreline.
  comb_dist$D2C <- apply(comb_dist[, 2:7], 1, FUN = min)
  comb_dist <- comb_dist %>% dplyr::select(ID, D2C)


  # update df data with D2C
  df <- merge(df, comb_dist, by = "ID")
}
