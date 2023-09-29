

fit_obs_pred = function(df ){
  
 # swe_name <- rlang::ensym(swe_name)
  
  # fit GEV distribution to  observations
  fit_gev <- extRemes::fevd(
    x = df$predicted_SWE,  type = "GEV",
    method = "Lmoments", time.units = "days"
  )
  
  
  # data frame of station parameters(GEV)
  df_para <- data.frame(
    ID = unique(df$ID),
    LOC_PRED = fit_gev[["results"]][["location"]],
    SCALE_PRED = fit_gev[["results"]][["scale"]],
    SHAPE_PRED = fit_gev[["results"]][["shape"]])
    
    return(df_para)
}


