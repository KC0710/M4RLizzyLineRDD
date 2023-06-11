#' Function to apply meteorological normalisation
#'
#' This is the main function to apply a gbm model to a data set.
#'
#' @param input_data Data frame to analyse. Must contain a POSIXct field called
#'   \code{date}.
#' @param vars Explanatory variables to use. These variables will be used to
#'   build the gbm model. Note that the model must include a trend component.
#'   Several variables can be automatically calculated (see [prepData()] for
#'   details).
#' @param pollutant The name of the variable to apply meteorological
#'   normalisation to.
#' @param sam.size The number of random samples to extract from the data for
#'   model building. While it is possible to use the full data set, for data
#'   sets spanning years the model building can take a very long time to run.
#'   Additionally, there will be diminishing returns in terms of model accuracy.
#'   If \code{sam.size} is greater than the number of number of rows of data,
#'   the number of rows of data is used instead.
#' @param n.trees Number of trees to fit.
#' @param simulate Should the original time series be randomly sampled with
#'   replacement? The default is \code{FALSE}. Setting \code{simulate = TRUE}
#'   can be useful for estimating model uncertainties. In which case models
#'   should be run multiple times with \code{B = 1} and a different value of
#'   \code{seed} e.g. \code{seed = runif(1)}.
#' @param B Number of bootstrap simulations for partial dependence plots.
#' @param n.core Number of cores to use for parallel processing.
#' @param seed Random number seed for reproducibility in returned model.
#' @export
#' @return Returns a list including the model, influence data frame and partial
#'   dependence data frame.
#' @author David Carslaw

buildModTuned <- function(input_data, gbm_hyperparams, prep.data = TRUE, vars = c(
  "trend", "ws", "wd", "hour",
  "weekday", "temp"
),
pollutant = "nox", sam.size = nrow(input_data),
simulate = FALSE,
B = 400, n.core = 16, seed = 123) {
  ## add other variables, select only those required for modelling
  if(prep.data){input_data <- prepData(input_data)}
  input_data <- dplyr::select(input_data, all_of(c("date", vars, pollutant)))
  input_data <- stats::na.omit(input_data) # only build model where all data are available - can always predict in gaps
  
  variables <- paste(vars, collapse = "+")
  eq <- stats::formula(paste(pollutant, "~", variables))
  
  # randomly sample data according to sam.size
  if (sam.size > nrow(input_data)) {
    sam.size <- nrow(input_data)
  }
  
  if (simulate) {
    id <- sample(nrow(input_data), size = sam.size, replace = TRUE)
    input_data <- input_data[id, ]
  } else {
    id <- sample(nrow(input_data), size = sam.size)
    input_data <- input_data[id, ]
  }
  
  ## if more than one simulation only return model ONCE
  if (B != 1L) {
    mod <- runGbmTuned(input_data, eq, gbm_hyperparams, vars,
                  return.mod = TRUE, simulate = simulate,
                  seed
    )
  }
  
  # if model needs to be run multiple times
  res <- partialDepTuned(input_data, eq, gbm_hyperparams, vars, B, n.core, seed)
  
  if (B != 1) Mod <- mod$model else Mod <- res[[3]]
  
  # return a list of model, data, partial deps
  result <- list(model = Mod, influence = res[[2]], data = input_data, pd = res[[1]])
  class(result) <- "deweather"
  
  return(result)
}

#' Extract PD
#' @noRd
extractPDTuned <- function(vars, mod) {
  n <- 100 ## resolution of output
  
  if ("trend" %in% vars) n <- 500
  
  if (vars %in% c("hour", "hour.local")) n <- 24
  
  ## extract partial dependence values
  res <- gbm::plot.gbm(mod, vars, continuous.resolution = n, return.grid = TRUE)
  res <- data.frame(
    y = res$y, var = vars, x = res[[vars]],
    var_type = ifelse(is.numeric(res[[vars]]), "numeric", "character")
  )
  
  return(res)
}

#' Run Gbm
#' @noRd
runGbmTuned <- function(dat, eq, gbm_hyperparams, vars, return.mod, simulate,
                   seed = seed) {
  ## sub-sample the data for bootstrapping
  if (simulate) {
    dat <- dat[sample(nrow(dat), nrow(dat), replace = TRUE), ]
  }
  
  # these models for AQ data are not very sensitive to tree sizes > 1000
  # make reproducible
  if (!simulate) set.seed(seed) else set.seed(stats::runif(1))
  
  mod <- gbm::gbm(eq,
                  data = dat, distribution = "gaussian", n.trees = gbm_hyperparams[5],
                  interaction.depth = gbm_hyperparams[1], 
                  n.minobsinnode = gbm_hyperparams[2],
                  shrinkage = gbm_hyperparams[3],
                  bag.fraction = gbm_hyperparams[4],
                  train.fraction = 1,
                  keep.data = TRUE, verbose = FALSE
  )
  
  ## extract partial dependnece componets
  
  pd <- lapply(vars, FUN=extractPDTuned, mod)
  pd <- do.call(rbind, pd)
  
  ## relative influence
  ri <- summary(mod, plotit = FALSE)
  ri$var <- stats::reorder(ri$var, ri$rel.inf)
  
  if (return.mod) {
    result <- list(pd = pd, ri = ri, model = mod)
    
    return(result)
  } else {
    return(list(pd, ri))
  }
}