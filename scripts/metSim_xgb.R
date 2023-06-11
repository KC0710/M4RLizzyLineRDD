#' Function to run random meteorological simulations on a gbm model
#'
#' @param dw_model Model object from running [buildMod()].
#' @param newdata Data set to which to apply the model. If missing the data used
#'   to build the model in the first place will be used.
#' @param metVars The variables that should be randomly varied.
#' @param n.core Number of cores to use.
#' @param B Number of simulations
#' @export
#' @return To add
#' @author Moi

metSim_xgb <- function(xgb_model, 
                       newdata,
                       metVars = c("ws", "wd", "temp"),
                       ntreelimit = 1000,
                       n.core = 16, B = 400) {
  if (!inherits(xgb_model, "xgb.Booster")) {
    stop("Need to supply an xgboost model object.")
  }
  
  #if (missing(newdata)) {
    #stop("Need to supply a data frame with new meteorological data.")
  #} else {
  #  newdata <- prepData(newdata)
  #}
  
  #cl <- parallel::makeCluster(n.core)
  #doParallel::registerDoParallel(cl)
  
  prediction <- foreach::foreach(
    i = 1:B, .inorder = FALSE, .combine = "rbind",
    .packages = c("xgboost", "dplyr", "tidyr"),
    .export = c("doPred", "prepdat")
  ) %do%
    doPred(newdata, xgb_model, metVars, ntreelimit)
  
  #parallel::stopCluster(cl)
  
  names(prediction)[2] <- "Value"
  
  ## Aggregate results
  prediction <- dplyr::group_by(prediction, .data$date) %>%
    dplyr::summarise(Value := mean(.data[["Value"]]))
  
  return(dplyr::tibble(prediction))
}


## randomly sample from original data
doPred <- function(mydata, mod, metVars, ntreelimit=1000) {
  ## random samples
  n <- nrow(mydata)
  id <- sample(1:n, n, replace = FALSE)
  
  ## new data with random samples
  mydata[metVars] <- lapply(mydata[metVars], function(x) x[id])
  
  mydataprep <- prepdat(mydata, metVars, train_phase=FALSE)
  
  prediction <- predict(mod, newdata = mydataprep, ntreelimit = ntreelimit)
  
  prediction <- data.frame(date = mydataprep$date, pred = prediction)
  
  return(prediction)
}