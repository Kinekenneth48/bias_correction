#This function is used to update data frame with ecoregion information.

ecoregion = function(ecoregions_all, df) {
  
  # Extract location coordinates  for stations
  sites <- unique(df[c("ID", "NAME", "LONGITUDE", "LATITUDE")]) %>%
    dplyr::filter(!is.na(LONGITUDE)) %>%
    as.data.frame(.)
  
  # get station location and ID
  sites_geo <- st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE")) %>%
    st_set_crs(., 4326)
  
  # check if crs of object is longlat
  st_is_longlat(sites_geo) 
  
  # re-project crs of site_geo data
  sites_geo <- sf::st_transform(sites_geo, crs(ecoregions_all))
  
  
  # use st_join to find intersection between two geometries
  ecoregion_id <- st_join(sites_geo, ecoregions_all) %>%
    na.omit(.) %>%
    dplyr::select(ID, NA_L3CODE, NA_L2CODE, NA_L1CODE)
  
  #drop geometries
  ecoregion_id = st_drop_geometry(ecoregion_id)
  
  
  #update combined data with ecoregions
  df = merge(df,  ecoregion_id, by="ID")
  
 return(df) 
}