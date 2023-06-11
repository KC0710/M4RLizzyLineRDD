library(here)
library(tidyr)
library(dplyr)
library(lubridate)
library(deweather)
library(mlr3verse)
library(doParallel)
library(xgboost)

source(here("LizzyLineRDD", "scripts", "metNormTuned.R"))
source(here("LizzyLineRDD", "scripts", "metSim_xgb.R"))
source(here("LizzyLineRDD", "scripts", "hvblockedfolds.R"))
source(here("LizzyLineRDD", "scripts", "hyperparameter_optimisation.R"))

load(here("LizzyLineRDD", "data", "processed", "all_validated_rr_modmat.RData"))
load(here("LizzyLineRDD", "data", "processed", "all_validated_rr_rdydw.RData"))
load(here("LizzyLineRDD", "results", "optim_runs_by_sitepol_sf.RData"))

second_fifty_rdydw <- all_validated_rr_rdydw[51:100]
hyp <- lapply(optim_runs_by_sitepol_sf, FUN = function(x) unlist(x$res$result_x_domain))

metNormTunedTry <- function(aq.dat, hyp){
 return(tryCatch(metNormTuned(aq.dat, hyp), error=function(e) NULL))
}

dwoptim_sf <- mapply(second_fifty_rdydw,
                     hyp,
                     FUN=metNormTuned)

save(dwoptim_sf, file=here("LizzyLineRDD", "results", "dwoptim_sf.RData"))