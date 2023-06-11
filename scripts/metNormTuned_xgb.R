prepdat <- function(aq.dat, metVars, train_phase=FALSE){
  aq.dat.prep <- aq.dat[c("Value", metVars)]
  aq.dat.prep$weekday <- as.numeric(factor(aq.dat.prep$weekday,
                                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                           labels = c(1,2,3,4,5)))
  aq.dat.prep$Hour <- as.numeric(aq.dat.prep$Hour)
  
  if (train_phase){
    aq.dat.prep <- aq.dat.prep %>% drop_na()
    dat <- xgboost::xgb.DMatrix(data=as.matrix(aq.dat.prep %>% select(-Value)),
                                label=as.matrix(aq.dat.prep$Value))
    return(dat)
  }
  
  dat <- xgboost::xgb.DMatrix(data=as.matrix(aq.dat.prep %>% select(-Value)))
                         
  return(dat)
}


metNormTuned_xgb <- function(aq.dat, gbm_hyperparams){
  
  metVars <- c("ws", "air_temp", "atmos_pres",
               "RH", "MOL", "weekday",
               "rain", "wd", "jday", "Hour")
  
  params <- list(max_depth = gbm_hyperparams[1],
                 min_child_weight = gbm_hyperparams[2],
                 eta = gbm_hyperparams[3],
                 colsample_bytree = gbm_hyperparams[4],
                 objective = "reg:squarederror",
                 eval_metric = "rmse")
  
  train <- prepdat(aq.dat, metVars, train_phase=TRUE)
  
  mod <- xgboost::xgb.train(data = train,
                            params = params,
                            nrounds = gbm_hyperparams[5])
                 
  metNorm <- metSim_xgb(mod,
                        newdata = aq.dat,
                        metVars = metVars,
                        ntreelimit=gbm_hyperparams[5])
  
  return(list(metNorm=metNorm))
}
