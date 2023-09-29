

keep_max_snwd <- function(df) {
  # Remove rows with NA values in column WESD or WESD_I
  df <- df[complete.cases(df[c("WESD", "WESD_I")]), ]

  # Convert the DATE column to a Date object
  df$DATE <- as.Date(df$DATE)

  # Create a new column SYEAR that represents the start year of the snow season
  df$SYEAR <- as.integer(format(lubridate::floor_date(df$DATE, unit = "year") +
    lubridate::years(if_else(lubridate::month(df$DATE) > 6, 1, 0)), "%Y"))

  df_max <- df %>%
    dplyr::group_by(ID, SYEAR) %>%
    dplyr::slice(which.max(WESD)) %>%
    dplyr::select(ID, SYEAR, WESD, WESD_I) %>%
    dplyr::ungroup() %>%
    dplyr::filter(WESD != 0, WESD_I != 0,)

  # Return the filtered data table
  return(df_max)
}
