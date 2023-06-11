runOptimMetNormBatch <- function(batch_optim, batch_dw, num){
  optim_runs_by_sitepol <- hypoptim(batch_optim, num)
  
  hyp <- lapply(optim_runs_by_sitepol, FUN = function(x) unlist(x$res$result_x_domain))
  
  #metNormTunedTry <- function(aq.dat, hyp){
   # return(tryCatch(metNormTuned_gbm(aq.dat, hyp), error=function(e) NULL))
  #}
  
  dwoptim_batch <- mapply(batch_dw,
                          hyp,
                          FUN=metNormTuned_gbm)
  
  metNormDay <- dwoptim_batch[1, ]
  
  train.error <- dwoptim_batch[2, ]
  
  pd <- dwoptim_batch[3, ]

  save(metNormDay,
       file=here("LizzyLineRDD", "results", paste0("dwoptim_batch", num, ".RData")))
  
  save(train.error,
       file=here("LizzyLineRDD", "results", paste0("gbm_rmse", num, ".RData")))
  
  save(pd,
       file=here("LizzyLineRDD", "results", paste0("pd", num, ".RData")))
}
