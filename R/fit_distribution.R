

fit_distribution <- function(df, column, dist_name = 'GEV') {
  require(extRemes)
  
  fit <- extRemes::fevd(df[[column]], type = dist_name,method = "Lmoments")
  
  # data frame of station parameters(GEV)
  df_para <- data.frame(
    ID = unique(df$ID),
    LOC_PRED = fit[["results"]][["location"]],
    SCALE_PRED = fit[["results"]][["scale"]],
    SHAPE_PRED = fit[["results"]][["shape"]])
  
  return(df_para)
}

