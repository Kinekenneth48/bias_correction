

fit_observations <- function(df, annual = TRUE, method = "block_maxima_vanilla",
                             threshold = 0, adapt.threshold = FALSE, n = 10000) {
  if (nrow(df[[1]]) < 30) {

    # remove data if observations are less than 30
    rm(df)
  } else if (annual == TRUE) {
    if (method == "block_maxima_vanilla") {

      # fit GEV distribution to  observations
      fit_gev <- extRemes::fevd(
        x = df[[1]]$pos_load, type = "GEV",
        method = "Lmoments", time.units = "days"
      )

      # estimate return level values
      y <- return.level(fit_gev, return.period = c(50, 100, 500))

      # data frame of station parameters(GEV)

      df <- station_para_gev(df, fit_gev, y)

      # convert from mm to psf
      df <- mm_to_psf(df)

      return(df)
    } else if (method == "block_maxima_zero") {

      # fit GEV distribution to  observations
      fit_gev <- extRemes::fevd(
        x = df[[1]]$pos_load, type = "GEV",
        method = "Lmoments", time.units = "days"
      )

      # get percentage of zero-valued maximums
      zero_percent <- (sum(df[[2]][["pos_load"]] == 0)) / (nrow(df[[2]]))

      p1 <- (0.98 - zero_percent) / (1 - zero_percent)
      p2 <- (0.99 - zero_percent) / (1 - zero_percent)
      p3 <- (0.998 - zero_percent) / (1 - zero_percent)

      # adjust quantile for percentage of zero-valued maximums
      y <- eva::qgev(
        p = c(p1, p2, p3),
        loc = fit_gev[["results"]][["location"]],
        scale = fit_gev[["results"]][["scale"]],
        shape = fit_gev[["results"]][["shape"]]
      )

      # data frame of station parameters(GEV)

      df <- station_para_gev(df, fit_gev, y)

      # convert from mm to psf
      df <- mm_to_psf(df)

      return(df)
    } else if (method == "block_maxima_annual") {
      df_pos <- df[[2]] %>%
        dplyr::filter(MAX > 0)

      # fit GEV distribution to  observations
      fit_gev <- extRemes::fevd(
        x = df_pos$MAX, type = "GEV",
        method = "Lmoments"
      )

      # get percentage of zero-valued maximums
      zero_percent <- (sum(df[[2]][["MAX"]] == 0)) / (nrow(df[[2]]))

      p1 <- (0.98 - zero_percent) / (1 - zero_percent)
      p2 <- (0.99 - zero_percent) / (1 - zero_percent)
      p3 <- (0.998 - zero_percent) / (1 - zero_percent)

      # adjust quantile for percentage of zero-valued maximums
      y <- eva::qgev(
        p = c(
          p = c(p1, p2, p3)
        ),
        loc = fit_gev[["results"]][["location"]],
        scale = fit_gev[["results"]][["scale"]],
        shape = fit_gev[["results"]][["shape"]]
      )

      # data frame of station parameters(GEV)

      df <- station_para_gev(df, fit_gev, y)


      # convert from mm to psf
      df <- mm_to_psf(df)

      return(df)
    }
  } else if (annual == FALSE) {
    if (method == "two_stage_gev") {

      # get count of positive changes per snow year
      counts_per_year <- df[[1]] %>%
        dplyr::group_by(SYEAR) %>%
        dplyr::summarise(count = n())

      mean_count <- mean(counts_per_year$count)

      # fit GEV distribution to generated observations
      fit_gev <- extRemes::fevd(
        x = df[[1]]$pos_load, type = "GEV",
        method = "Lmoments", time.units = "days"
      )

      r1 <- (1 - (1 / (50 * mean_count)))
      r2 <- (1 - (1 / (100 * mean_count)))
      r3 <- (1 - (1 / (500 * mean_count)))

      y <- eva::qgev(
        p = c(r1, r2, r3), loc = fit_gev[["results"]][["location"]],
        scale = fit_gev[["results"]][["scale"]],
        shape = fit_gev[["results"]][["shape"]]
      )

      # data frame of station parameters(GEV)

      df <- station_para_gev(df, fit_gev, y)

      # convert from mm to psf
      df <- mm_to_psf(df)

      return(df)
    } else if (method == "two_stage_gamma") {

      # get count of positive changes per snow year
      counts_per_year <- df[[1]] %>%
        dplyr::group_by(SYEAR) %>%
        dplyr::summarise(count = n())

      # fit poisson distribution to count data to get lambda
      mean_count <- mean(counts_per_year$count)

      # fit a gamma distribution to data
      fit_gamma <- fitdistrplus::fitdist(df[[1]]$pos_load,
        distr = "gamma",
        method = "mle", lower = c(shape = 0, rate = 0, scale = 0),
        start = list(rate = 0.2, shape = 0.2)
      )

      # create probabilities for 50, 100, and 500 years quantile adjusted for the mean count
      r1 <- (1 - (1 / (50 * mean_count)))
      r2 <- (1 - (1 / (100 * mean_count)))
      r3 <- (1 - (1 / (500 * mean_count)))


      y <- stats::qgamma(
        p = c(r1, r2, r3), shape = fit_gamma[["estimate"]][["shape"]],
        rate = fit_gamma[["estimate"]][["rate"]]
      )

      df <- data.frame(
        ID = unique(df[[1]]$ID),
        STATION = unique(df[[1]]$NAME),
        STATE = unique(df[[1]]$STATE),
        DAY_CHANGE = unique(df[[1]]$DAY_CHANGE),
        SHAPE = fit_gamma[["estimate"]][["shape"]],
        SCALE = (1 / fit_gamma[["estimate"]][["rate"]]),
        EVENT50 = y[1],
        EVENT100 = y[2],
        EVENT500 = y[3]
      )

      # convert from mm to psf
      df <- mm_to_psf(df)

      return(df)
    } else if (method == "GP") {

      # get MRI from the GP distribution with threshold
      # mri_gpd <- mri_qgpd(df, adapt.threshold, threshold)

      mri_gpd <- tryCatch(mri_qgpd(df, adapt.threshold, threshold),
        error = function(e) NULL
      )

      if (is.null(mri_gpd)) {
        df <- data.frame(
          ID = unique(df[[1]]$ID),
          STATION = unique(df[[1]]$NAME),
          STATE = unique(df[[1]]$STATE),
          DAY_CHANGE = unique(df[[1]]$DAY_CHANGE),
          K = NA,
          SCALE = NA,
          SHAPE = NA,
          EVENT50 = NA,
          EVENT100 = NA,
          EVENT500 = NA,
          TAIL = NA
        )
      } else {
        df <- data.frame(
          ID = unique(df[[1]]$ID),
          STATION = unique(df[[1]]$NAME),
          STATE = unique(df[[1]]$STATE),
          DAY_CHANGE = unique(df[[1]]$DAY_CHANGE),
          K = mri_gpd[4],
          SCALE = mri_gpd[5],
          SHAPE = mri_gpd[6],
          EVENT50 = mri_gpd[1],
          EVENT100 = mri_gpd[2],
          EVENT500 = mri_gpd[3],
          TAIL = mri_gpd[7]
        )

        # convert from mm to psf
        df <- mm_to_psf(df)
      }
      return(df)
    }
  }
}
