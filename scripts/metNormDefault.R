metNormDefault <- function(aq.dat){
  
  metVars <- c("ws", "air_temp", "atmos_pres",
               "RH", "MOL", "weekday", "trend",
               "rain", "wd", "jday", "Hour")
  
  mod_pol <- deweather::buildMod(aq.dat,
                                 vars = metVars,
                                 pollutant = "Value",
                                 n.trees = 1000,
                                 n.core = 16)
  
  rmse <- sqrt(mod_pol$model$train.error[1000])
  
  return(list(error=rmse))
}
