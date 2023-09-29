convert_sf <- function(df) {
  sites_sf <- sf::st_as_sf(
    x = df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326
  )
}