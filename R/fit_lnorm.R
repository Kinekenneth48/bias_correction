
fit_lnorm <- function(df, column,...) {
  require(fitdistrplus)
  
  data = as.numeric(df[[column]])
  
  fit <- fitdistrplus::fitdist(
    data = data, distr = "lnorm",
    start = list(
      meanlog = mean(log(data)), sdlog = stats::sd(log(data))
    ),
    method = "mle"
  )
    
  
  # data frame of station parameters(GEV)
  df_para <- data.frame(
    ID = unique(df$ID),
    meanlog = fit[["estimate"]][["meanlog"]],
    sdlog = fit[["estimate"]][["sdlog"]]
  )
  
  return(df_para)
}

