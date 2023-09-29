# get climate information
get_climate <- function(df) {
  climate <- climateR::getGridMET(
    AOI = df,
    param = c("tmin", "tmax", "vpd", "wind_vel", "srad"),
    startDate = df$startDate,
    endDate = df$endDate
  ) %>%
    dplyr::mutate(ID = df$ID)
}