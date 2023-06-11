run_hyperparameter_optimisation <- function(aq.dat){
  site <- aq.dat$Site[1]
  pol <- aq.dat$Pollutant[1]

  aq.dat_m <- aq.dat %>% select(-Site, -Pollutant)
  
  poe <- po("encode")
  task_aq <- as_task_regr(aq.dat_m, target = "Value", id="aq_bayesopt")
  aq.dat_moh <- poe$train(list(task_aq))[[1]]$data() #one hot encode factor variables
  task_aq <- as_task_regr(aq.dat_moh, target = "Value", id="aq_bayesopt")
  
  hv <- rsmp("custom") # hvblocked cross validation
  folds <- hvblockedfolds(aq.dat_moh, block.size = 48)
  hv$instantiate(task_aq, folds$train.id, folds$test.id) 
  
  learner <- lrn("regr.gbm")

  search_space <- ps(
    interaction.depth = p_int(4, 12),
    n.minobsinnode = p_int(5, 15),
    shrinkage = p_dbl(0.05, 0.5, logscale=TRUE),
    bag.fraction = p_dbl(0.4, 0.9),
    n.trees = p_int(900, 1500))
  
  surrogate = mlr3mbo::srlrn(lrn("regr.km",
                        covtype = "matern3_2",
                        optim.method = "gen",
                        nugget.stability = 10^-8,
                        control = list(trace = FALSE)))
  acq_function = mlr3mbo::acqf("ei")
  acq_optimizer = mlr3mbo::acqo(bbotk::opt("random_search", batch_size = 10),
                       terminator = trm("evals", n_evals = 150))
  
  instance <- ti(
    task = task_aq,
    learner = learner,
    resampling = hv,
    measures = msr("regr.rmse"),
    search_space = search_space,
    terminator = trm("run_time", secs=7200))
  
  tuner <- tnr("mbo",
               loop_function = mlr3mbo::bayesopt_ego,
               acq_optimizer = acq_optimizer,
               acq_function = acq_function,
               surrogate = surrogate)
  
  tuner$optimize(instance)

  return(list(site=site, pollutant=pol, res=instance))
}