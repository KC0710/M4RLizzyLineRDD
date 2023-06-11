library(here)
library(tidyr)
library(dplyr)
library(lubridate)
library(deweather)
library(mlr3verse)
library(doParallel)
library(xgboost)

source(here("LizzyLineRDD", "scripts", "hvblockedfolds.R"))
source(here("LizzyLineRDD", "scripts", "hyperparameter_optimisation.R"))

load(here("LizzyLineRDD", "data", "processed", "all_validated_rr_modmat.RData"))

# HyperparamOptim ---------------------------------------------------------

second_fifty <- all_validated_rr_modmat[51:100]

hypoptim <- function(batch) {
  
  set.seed(7832)

  hypoptimTry <- function(aq.dat){
    tryCatch(run_hyperparameter_optimisation(aq.dat),
             error=function(e) NULL)
  }
  
  # Loop over input data
  result <- lapply(batch, FUN = run_hyperparameter_optimisation)
  
  # Return result
  return(result)
}

optim_runs_by_sitepol_sf <- hypoptim()

save(optim_runs_by_sitepol_sf, file=here("LizzyLineRDD", "results", "optim_runs_by_sitepol_sf.RData"))