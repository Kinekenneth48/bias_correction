
get_climate_station = function(data_sf){
  
  df <- try(
    climateR::getGridMET(
      AOI = data_sf,
      param = c("tmin", "tmax", "vpd", "wind_vel", "srad"),
      startDate = data_sf$startDate,
      endDate = data_sf$endDate
    ) %>%
      dplyr::mutate(ID = data_sf$ID)
  )
  
  return(df)
}
