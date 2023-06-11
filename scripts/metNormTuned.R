source(here("LizzyLineRDD", "scripts", "buildModTuned.R"))
source(here("LizzyLineRDD", "scripts", "partialDepTuned.R"))

metNormTuned_gbm <- function(aq.dat, gbm_hyperparams){
  
  metVars <- c("trend", "ws", "air_temp", "atmos_pres",
               "RH", "MOL", "weekday",
               "rain", "wd", "jday", "Hour")
  
  mod <- buildModTuned(aq.dat,
                       gbm_hyperparams,
                       prep.data=FALSE,
                       vars=metVars,
                       pollutant="Value")
  
  gc()
  
  rmse <- sqrt(mod$model$train.error[gbm_hyperparams[5]])
  
  metSimTry <- function(mod, newdata, metVars, n.core, B){
    tryCatch(deweather::metSim(mod,
                               newdata,
                               metVars,
                               n.core,
                               B),
             error=function(e) NULL)
  }
  
  metNorm <- openair::timeAverage(metSimTry(mod,
                                            newdata=aq.dat,
                                            metVars=metVars[-1],
                                            n.core=6,
                                            B=200))
  
  return(list(metNorm=metNorm, error=rmse, pd=mod$pd))
}