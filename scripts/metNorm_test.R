library(deweather)
library(gbm)
library(ggplot2)
library(here)
library(dplyr)
library(doParallel)

source(here("LizzyLineRDD", "scripts", "metNorm.R"))
source(here("LizzyLineRDD", "scripts", "buildMod.R"))
source(here("LizzyLineRDD", "scripts", "partialDep.R"))
load(here("LizzyLineRDD", "results", "res_hypoptim_test.RData"))
load(here("LizzyLineRDD", "data", "processed", "all_validated_rr.RData"))
load(here("LizzyLineRDD", "data", "processed", "meteo_full.RData"))

df <- split(all_validated_rr[[1]], all_validated_rr[[1]]$Pollutant)[[1]]

seasonality_vars <- c("weekday", "jday", "trend")
meteo_seasonality <- prepData(meteo_full, add=seasonality_vars) %>%
  select(-rain_12, -rain_06, -rain_06_modify,
         -wd_cos, -wd_sin, -wd_math,
         -dew_point, -ceil_hgt, -visibility)

dw <- meterological_normalisation(df, res, meteo_seasonality)
save(dw, file=here("LizzyLineRDD", "results", "deweathered.RData"))

