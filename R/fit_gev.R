

fit_gev <- function(df, column) {
  fit <- extRemes::fevd(df[[column]], type = "GEV", method = c("Lmoments"))

  df <- data.frame(
    loc = fit$results[["location"]],
    scale = fit$results[["scale"]],
    shape = fit$results[["shape"]]
  )

  return(df)
}
