library(here)
library(deweather)
library(dplyr)

source(here("LizzyLineRDD", "scripts", "metNormDefault.R"))
load(here("LizzyLineRDD", "data", "processed", "all_validated_rr_rdydw.RData"))

metNormDefaultTry <- function(aq.dat){
  return(tryCatch(metNormDefault(aq.dat), error=function(e) NULL))
}

dwdefault <- lapply(all_validated_rr_rdydw, FUN = metNormDefaultTry)

save(dwdefault, file=here("LizzyLineRDD", "results", "dwdefault_rmse.RData"))
