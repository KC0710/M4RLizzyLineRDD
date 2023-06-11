gbm.cverr.earlystopping <- function(
  # Necessary input
  x, y, distribution = "gaussian", cv.train.folds,
  cv.test.folds,
  
  # GBM metaparameters
  interaction.depth = 1, # number or vector
  n.minobsinnode = 10, # number or vector
  shrinkage = 0.001, # number or vector
  bag.fraction = 0.5, # number or vector
  
  w = NULL, # vector or list of vectors
  var.monotone = NULL, # vector or list of vectors
  #feature.fraction = 0.8, #number of vector
  
  # Find max number of trees
  max.trees = 300,
  
  # Time management
  n.cores = 16, max.time = NULL,
  early.stop.rounds = 10,
  
  # Verbosity
  verbose = TRUE,
  
  # Reproducible results
  seed = NULL){
  
  require(gbm)

  # Add trees to the model until early stopping criterion is met or max.trees is reached
  best_error <- Inf
  for (tree in 1:max.trees) {
    error <- gbm.cverr(x,
                       y,
                       cv.train.folds,
                       cv.test.folds,
                       distribution = "gaussian",
                       w = NULL, 
                       var.monotone = NULL,
                       n.trees = tree, 
                       interaction.depth = interaction.depth,
                       n.minobsinnode = n.minobsinnode, 
                       shrinkage = shrinkage, 
                       bag.fraction = bag.fraction,
                       n.cores = n.cores
                       )
    
    if (error < best_error){
      best_error <- error
      best_ntree <- tree
    } else if (tree - best_ntree >= early.stop.rounds) {
      break
    }
  }
  return(best_error)
}