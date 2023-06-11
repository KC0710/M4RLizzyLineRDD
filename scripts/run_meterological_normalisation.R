library(deweather)
library(gbm)
library(ggplot2)
library(here)
library(dplyr)
library(doParallel)

source(here("LizzyLineRDD", "scripts", "metNorm.R"))
source(here("LizzyLineRDD", "scripts", "buildMod.R"))
source(here("LizzyLineRDD", "scripts", "partialDep.R"))
load(here("LizzyLineRDD", "results", "optimal_hyperparams_by_site.RData"))
load(here("LizzyLineRDD", "data", "processed", "all_validated_rr.RData"))
load(here("LizzyLineRDD", "data", "processed", "meteo_seasonality.RData"))

all_validated_rr_split <- lapply(all_validated_rr, FUN=function(x){split(x, x$Pollutant)})

res <- mapply(FUN=function(aq.dat, hyp.param){meterological_normalisation(aq.dat,
                                                                          hyp.param,
                                                                          meteo_seasonality)},
  purrr::flatten(all_validated_rr_split),
  purrr::flatten(optim_runs_by_site))

save(res, file=here("LizzyLineRDD", "results", "deweathered.RData"))
  