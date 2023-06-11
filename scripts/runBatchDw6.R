library(here)
library(tidyr)
library(dplyr)
library(lubridate)
library(deweather)
library(mlr3verse)
library(doParallel)
library(openair)
library(xgboost)

source(here("LizzyLineRDD", "scripts", "metNormTuned.R"))
source(here("LizzyLineRDD", "scripts", "metSim_xgb.R"))
source(here("LizzyLineRDD", "scripts", "hvblockedfolds.R"))
source(here("LizzyLineRDD", "scripts", "hyperparameter_optimisation.R"))
source(here("LizzyLineRDD", "scripts", "runOptimMetNormBatch.R"))
source(here("LizzyLineRDD", "scripts", "hypoptim.R"))

load(here("LizzyLineRDD", "data", "processed", "all_validated_rr_modmat.RData"))
load(here("LizzyLineRDD", "data", "processed", "all_validated_rr_rdydw.RData"))

batch_length <- 50
nsitepol <- length(all_validated_rr_modmat)
split_vec <- split(1:nsitepol,           
                   ceiling(seq_along(1:nsitepol) / batch_length))
num_batch <- length(split_vec)

runOptimMetNormBatch(all_validated_rr_modmat[split_vec[[6]]],
                     all_validated_rr_rdydw[split_vec[[6]]],
                     6)
