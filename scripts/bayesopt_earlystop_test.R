
# Load --------------------------------------------------------------------

library(mlr3)
library(mlr3mbo)
library(mlr3learners)
library(bbotk)
library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(gbm)
library(deweather)
library(xgboost)

source(here("scripts", "helper.R"))
source(here("scripts", "hvblockedfolds.R"))
source(here("scripts", "gbm.cverr.R"))
source(here("scripts", "hyperparameter_optimisation.R"))

load(here("data", "processed", "all_validated_sites.RData"))
load(here("data", "processed", "meteo_full.RData"))

seasonality_vars <- c("weekday", "jday", "trend")
meteo_seasonality <- prepData(meteo_full, add=seasonality_vars) %>%
  select(-rain_12, -rain_06, -rain_06_modify,
         -wd_cos, -wd_sin, -wd_math,
         -dew_point, -ceil_hgt, -visibility)

all_validated_rr <- remove_recent(all_validated)

df <- all_validated_rr[[1]]
df_split <- split(df, df$Pollutant)
df_exp <- df_split[[1]]

inputs <- gbm_inputs(df_exp, meteo_seasonality)


# Create BayesOpt setting -------------------------------------------------
set.seed(42)
# Define the objective function to be optimized
obj_fun = function(xs) {
  # Set the seed for reproducibility
  set.seed(42)

  X <- inputs$modmat
  y <- inputs$yobs
  cv.train.folds <- inputs$train.id
  cv.test.folds <- inputs$test.id
  
  M <- xgb.DMatrix(data=X, label=y)
  # Use xgboost cross validation with early stopping.
  cvobj <- xgb.cv(params = list(
    booster          = "gbtree",
    learning_rate    = xs["learning_rate"],
    max_depth        = xs["max_depth"],
    min_child_weight = xs["min_child_weight"],
    colsample_bytree = xs["colsample_bytree"],
    objective        = 'reg:squarederror',
    train_folds      = cv.train.folds,
    folds            = cv.test.folds),
    data = M, ## must set in global.Env()
    nfold = 5,
    nround = 1000, ## Set this large and use early stopping
    nthread = 26, ## Adjust based on your machine
    prediction = FALSE,
    showsd = TRUE,
    early_stopping_rounds = 25, ## If evaluation metric does not improve on out-of-fold sample for 25 rounds, stop
    verbose = 1,
    print_every_n = 100
    )
  cv_err <- cvobj$evaluation_log %>% pull(4) %>% min 
  # Return the cross-validation error
  return(cv_err)
}

domain <- ParamSet$new(list(
  ParamInt$new("max_depth", lower = 1, upper = 10),
  ParamInt$new("min_child_weight", lower = 5, upper = 15),
  ParamDbl$new("learning_rate", lower = 0.001, upper = 0.5),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1))
)

codomain <- ps(y = p_dbl(tags = "minimize"))

obfun = ObjectiveRFun$new(
  fun = obj_fun,
  domain = domain,
  codomain = codomain,
)

instance = OptimInstanceSingleCrit$new(
  objective = obfun,
  terminator = trm("evals", n_evals = 100)
)

initial_design <- generate_design_lhs(domain, n=10)$data
instance$eval_batch(initial_design)

surrogate = srlrn(lrn("regr.km",
                      covtype = "matern3_2",
                      optim.method = "gen",
                      nugget.stability = 10^-8,
                      control = list(trace = FALSE)))
acq_function = acqf("ei")
acq_optimizer = acqo(opt("random_search", batch_size = 100),
                     terminator = trm("evals", n_evals = 20))
optimizer = opt("mbo",
                loop_function = bayesopt_ego,
                surrogate = surrogate,
                acq_function = acq_function,
                acq_optimizer = acq_optimizer)


res <- optimizer$optimize(instance)

save(res, file=here("LizzyLineRDD", "results", "res_hypoptim_test.RData"))
  