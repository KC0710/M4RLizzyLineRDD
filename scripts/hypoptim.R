hypoptim <- function(batch, num) {
  
  set.seed(7832)
  
  hypoptimTry <- function(aq.dat){
    tryCatch(run_hyperparameter_optimisation(aq.dat),
             error=function(e) NULL)
  }
  
  # Loop over input data
  result <- lapply(batch, FUN = run_hyperparameter_optimisation)
  
  perf <- lapply(result, 
                 FUN = function(x) c(x$site, x$pollutant, x$res$result$x_domain))

  save(perf,
       file = here("LizzyLineRDD", "results", paste0("bsthyp_batch", num, ".RData")))
  
  # Return result
  return(result)
}