#'runfolds
#'@keywords internal
#'@importFrom purrr map
#'@examples
#' 
#' j <- 1 # running the first repetition of 'currentSample', normally we are 
#' # calling an apply call to seq_along nFolds of repetition
#' # see also 'runreps()'
#' data <- ecuador
#' imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
#' currentSample <- readRDS("inst/test-objects/resamp.rda")[[1]]
#' currentRes <- readRDS("inst/test-objects/currentRes.rda")
#' 
#' runfolds_single <- runfolds(j = 1, data = ecuador, currentSample = currentSample,
#' formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
#' model.args = list(family = "binomial"), do.try = FALSE, model.fun = glm,
#' error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, 
#' imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
#' err.train = TRUE, importance = TRUE, currentRes = currentRes, 
#' pred.args = list(type = "response"), response = "slides", par.cl = 2, 
#' coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
#' pooled.obs.test = c(), err.fun = err.default)
#' 
#' ### rpart example
#' data(ecuador)
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' 
#' mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
#' ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
#' 
#' # Non-spatial 5-repeated 10-fold cross-validation:
#' mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
#' 
#' runfolds_single <- runfolds(j = 1, data = ecuador, currentSample = currentSample,
#' formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
#' do.try = FALSE, model.fun = rpart,
#' error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, pred.fun = mypred.rpart, 
#' model.args = list(control = ctrl),
#' imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
#' err.train = TRUE, importance = TRUE, currentRes = currentRes, 
#' response = "slides", par.cl = 2, 
#' coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
#' pooled.obs.test = c(), err.fun = err.default)
#' 
#' runfolds_list <- map(seq_along(1:4), function(rep) runfolds(j = rep, data = ecuador, currentSample = currentSample,
#' formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
#' do.try = FALSE, model.fun = rpart,
#' error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, pred.fun = mypred.rpart, 
#' model.args = list(control = ctrl),
#' imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
#' err.train = TRUE, importance = TRUE, currentRes = currentRes, 
#' response = "slides", par.cl = 2, 
#' coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
#' pooled.obs.test = c(), err.fun = err.default))
#' 
#' # create list with multiple fold results
#' runfolds_list <- map(seq_along(1:4), function(rep) runfolds(j = rep, data = ecuador, currentSample = currentSample,
#' formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
#' model.args = list(family = "binomial"), do.try = FALSE, model.fun = glm,
#' error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, 
#' imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
#' err.train = TRUE, importance = TRUE, currentRes = currentRes, 
#' pred.args = list(type = "response"), response = "slides", par.cl = 2, 
#' coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
#' pooled.obs.test = c(), err.fun = err.default))
#' 
#' # MERGE SUBLISTS: ONLY CURRENTRES AND CURRENTIMPO
#' 
#'@export

runfolds <- function(j = NULL, currentSample = NULL, data = NULL, formula = NULL, 
                     model.args = NULL, par.cl = NULL, par.mode = NULL,
                     do.try = NULL, model.fun = NULL, error.fold = NULL, 
                     error.rep = NULL, pred.fun = NULL, imp.variables = NULL,
                     imp.permutations = NULL, err.fun = NULL, train.fun = NULL,
                     err.train = NULL, importance = NULL, currentRes = NULL,
                     currentImpo = NULL, pred.args = NULL, progress = NULL, 
                     pooled.obs.train = NULL, pooled.obs.test = NULL, pooled.pred.train = NULL,
                     response = NULL, is.factor.prediction = NULL, pooled.pred.test = NULL,
                     coords = NULL, test.fun = NULL, imp.one.rep = NULL, i = NULL) { 
  if (importance == FALSE) {
    
    if (par.mode == 2 && progress == TRUE | progress == 1) {
      cat(date(), "Repetition", names(currentSample)[i], "- Fold", j, "\n")
    }
    if (par.mode == 2 && progress == TRUE | progress == 2) {
      cat(date(), "Repetition", names(currentSample)[i], "\n")
    }
  }

  # Create training sample:
  nd <- data[currentSample[[j]]$train, ]
  if (!is.null(train.fun))
    nd <- train.fun(data = nd, param = train.param)
  
  # Train model on training sample:
  margs <- c(list(formula = formula, data = nd), model.args)
  
  if (do.try)
  {
    fit <- try(do.call(model.fun, args = margs))
    
    # Error handling:
    if (class(fit) == "try-error")
    {
      fit <- NULL
      if (error.fold)
      {
        if (err.train)
        {
          currentRes[[j]]$train <- NULL
          # res[[i]][[j]]$train = NULL
          currentRes[[j]]$test <- NULL
          # res[[i]][[j]]$test =
        }
        if (importance)
        {
          currentImpo[[j]] <- c()
        }
      }
      if (do.gc >= 2)
        gc()
      next  # skip this fold
    }
    
  } else
  {
    fit <- do.call(model.fun, args = margs)
  }
  
  if (err.train == TRUE) {
    # Apply model to training sample:
    pargs <- c(list(object = fit, newdata = nd), pred.args)
    if (is.null(pred.fun))
    {
      pred.train <- do.call(predict, args = pargs)
    } else
    {
      pred.train <- do.call(pred.fun, args = pargs)
    }
    rm(pargs)
    
    # Calculate error measures on training sample:
    if (error.fold == TRUE) {
      if (do.try)
      {
        err.try <- try(err.fun(nd[, response], pred.train))
        if (class(err.try) == "try-error") {
          err.try <- NULL
        }
        currentRes[[j]]$train <- err.try  #res[[i]][[j]]$train = err.try
      } else {
        currentRes[[j]]$train <- err.fun(nd[, response], pred.train)  #res[[i]][[j]]$train = err.fun(nd[,response], pred.train)
      }
    }
    if (error.rep == TRUE) {
      pooled.obs.train <- c(pooled.obs.train, nd[, response])
      pooled.pred.train <- c(pooled.pred.train, pred.train)
    }
  } else {
    if (error.fold == TRUE) {
      currentRes[[j]]$train <- NULL  #res[[i]][[j]]$train = NULL
    }
  }
  
  # Create test sample:
  nd <- data[currentSample[[j]]$test, ]
  if (!is.null(test.fun))
    nd <- test.fun(data = nd, param = test.param)
  # Create a 'backup' copy for variable importance assessment:
  if (importance)
  {
    nd.bak <- nd
  }
  # Apply model to test sample:
  pargs <- c(list(object = fit, newdata = nd), pred.args)
  if (is.null(pred.fun))
  {
    pred.test <- do.call(predict, args = pargs)
  } else
  {
    pred.test <- do.call(pred.fun, args = pargs)
  }
  rm(pargs)
  
  # Calculate error measures on test sample:
  if (error.fold)
  {
    if (do.try)
    {
      err.try <- try(err.fun(nd[, response], pred.test))
      if (class(err.try) == "try-error")
        err.try <- NULL
      currentRes[[j]]$test <- err.try  #res[[i]][[j]]$test = err.try
    } else
    {
      currentRes[[j]]$test <- err.fun(nd[, response], pred.test)  #res[[i]][[j]]$test  = err.fun(nd[,response], pred.test)
    }
  }
  if (error.rep)
  {
    pooled.obs.test <- c(pooled.obs.test, nd[, response])
    pooled.pred.test <- c(pooled.pred.test, pred.test)
    # assign to outer scope; otherwise object is NULL in runreps
    is.factor.prediction <<- is.factor(pred.test)
  }
  
  ### Permutation-based variable importance assessment:
  if (importance & error.fold)
  {
    if (is.null(currentRes[[j]]$test)) {
      currentImpo[[j]] <- c()
      if (!progress == FALSE) {
        # cat(date(), "-- skipping variable importance\n")
      }
    } else {
      if (!progress == FALSE) {
        # cat(date(), "-- Variable importance\n")
      }
      imp.temp <- imp.one.rep
      
      # Parallelize this: ???
      for (cnt in 1:imp.permutations) {
        # Some output on screen:
        if (!progress == FALSE & (cnt > 1)) {
          if (log10(cnt) == floor(log10(cnt))) {
            #cat(date(), "   ", cnt, "\n")
            cat(date(), "Repetition", names(currentSample)[i], "- Fold", j, "- permutation-count:", cnt, "\n")
          }
        }
        # Permutation indices:
        permut <- sample(1:nrow(nd), replace = FALSE)
        
        # For each variable:
        for (vnm in imp.variables) {
          # Get undisturbed backup copy of test sample:
          nd <- nd.bak
          # Permute variable vnm:
          nd[, vnm] <- nd[, vnm][permut]
          # Apply model to perturbed test sample:
          pargs <- c(list(object = fit, newdata = nd), pred.args)
          if (is.null(pred.fun)) {
            pred.test <- do.call(predict, args = pargs)
          } else {
            pred.test <- do.call(pred.fun, args = pargs)
          }
          rm(pargs)
          
          # Calculate variable importance:
          if (do.try) {
            permut.err <- try(err.fun(nd[, response], pred.test))
            if (class(permut.err) == "try-error") {
              imp.temp[[vnm]][[cnt]] <- c()  # ???
            } else {
              imp.temp[[vnm]][[cnt]] <- as.list(unlist(currentRes[[j]]$test) -
                                                  unlist(permut.err))
              # as.list( unlist(res[[i]][[j]]$test) - unlist(permut.err) ) (apply '-' to
              # corresponding list elements; only works if all list elements are scalars)
            }
          } else {
            permut.err <- err.fun(nd[, response], pred.test)
            imp.temp[[vnm]][[cnt]] <- as.list(unlist(currentRes[[j]]$test) -
                                                unlist(permut.err))
            # as.list( unlist(res[[i]][[j]]$test) - unlist(permut.err) ) (apply '-' to
            # corresponding list elements; only works if all list elements are scalars)
          }
        }
      }
      # average the results obtained in each permutation:
      currentImpo[[j]] <- as.data.frame(t(sapply(imp.temp, function(y) sapply(as.data.frame(t(sapply(y,
                                                                                                     as.data.frame))), function(x) mean(unlist(x))))))
      rm(nd.bak, nd)  # better safe than sorry...
    }  # end of else if (!is.null(currentres[[j]]$test))
  }
  
  currentRes <- currentRes[[j]]
  currentImpo <- currentImpo[[j]]
  
  return(list(pooled.obs.train = pooled.obs.train,
              pooled.obs.test = pooled.obs.test,
              pooled.pred.train = pooled.pred.train,
              pooled.pred.test = pooled.pred.test,
              currentRes = currentRes,
              currentImpo = currentImpo))
  
}

#'
#'runreps
#'@keywords internal
#' j <- 1 # running the first repetition of 'currentSample', normally we are 
#' # calling an apply call to seq_along nFolds of repetition
#' # see also 'runreps()'
#' #### 2 repetitions, 4 folds
#' data <- ecuador
#' imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
#' currentSample <- readRDS("inst/test-objects/resamp.rda")
#' currentRes <- readRDS("inst/test-objects/currentRes.rda")
#' 
#' @examples 
#' 
#' data <- ecuador
#' imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
#' currentSample <- readRDS("inst/test-objects/resamp.rda")
#' currentRes <- readRDS("inst/test-objects/currentRes.rda")
#' 
#' runreps_res <- lapply(currentSample, function(X) runreps(currentSample = X, data = ecuador,
#' formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
#' model.args = list(family = "binomial"), do.try = FALSE, model.fun = glm,
#' error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, do.gc = 1,
#' imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
#' err.train = TRUE, importance = TRUE, currentRes = currentRes, 
#' pred.args = list(type = "response"), response = "slides", par.cl = 2, 
#' coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
#' pooled.obs.test = c(), err.fun = err.default))
#' 
#' runreps_res <- lapply(currentSample, function(X) runreps(currentSample = X, data = ecuador,
#' formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
#' do.try = FALSE, model.fun = rpart,
#' error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, pred.fun = mypred.rpart, 
#' model.args = list(control = ctrl),
#' imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
#' err.train = TRUE, importance = TRUE, currentRes = currentRes, 
#' response = "slides", par.cl = 2, 
#' coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
#' pooled.obs.test = c(), err.fun = err.default))
#' 
#' runfolds_list <- map(seq_along(1:4), function(j) runfolds(j))
#'@export
#'

# runreps function for lapply()
runreps <- function(currentSample = NULL, data = NULL, formula = NULL, 
                    model.args = NULL, par.cl = NULL, do.gc = 1, imp.one.rep = NULL,
                    do.try = NULL, model.fun = NULL, error.fold = NULL, 
                    error.rep = NULL, pred.fun = NULL, imp.variables = NULL,
                    imp.permutations = NULL, err.fun = NULL, train.fun = NULL,
                    err.train = NULL, importance = NULL, currentRes = NULL,
                    currentImpo = NULL, pred.args = NULL, progress = NULL, 
                    pooled.obs.train = NULL, pooled.obs.test = NULL, pooled.pred.train = NULL,
                    response = NULL, is.factor.prediction = NULL, pooled.pred.test = NULL,
                    coords = NULL, test.fun = NULL, par.mode = NULL) {
  # output data structures
  currentRes <- NULL
  currentImpo <- currentSample
  currentPooled.err <- NULL
  
  if (error.fold) {
    currentRes <- lapply(currentSample, unclass)
    class(currentRes) <- "sperroresterror"
  } else {
    currentRes <- NULL
  }
  
  # Collect pooled results in these data structures:
  if (err.train) {
    pooled.obs.train <- pooled.pred.train <- c()
  }
  pooled.obs.test <- pooled.pred.test <- c()
  
  # do fold calculation for every repetition
  # returns list of length 6 which is handed over to runreps
  # current sample arriving here has length of folds (because it it alsready indexed by
  # apply call of runreps)
  
  # this ensures that runfolds finds all objects which have been defined until here
  environment(runfolds) <- environment()
  
  runfolds_list <- map(seq_along(currentSample), function(rep) runfolds(j = rep, data = data, currentSample = currentSample,
                                                                      formula = formula, par.mode = par.mode, pred.fun = pred.fun,
                                                                      model.args = model.args, do.try = do.try, model.fun = model.fun,
                                                                      error.fold = error.fold, error.rep = error.rep, imp.permutations = imp.permutations, 
                                                                      imp.variables = imp.variables, is.factor.prediction = is.factor.prediction,
                                                                      err.train = err.train, importance = importance, currentRes = currentRes, 
                                                                      pred.args = pred.args, response = response, par.cl = par.cl, 
                                                                      coords = coords, progress = progress, pooled.obs.train = pooled.obs.train, 
                                                                      pooled.obs.test = pooled.obs.test, err.fun = err.fun))
  
  # how to subset lists
  # list_sub <- map(runfolds_list, function(x) x[c(5, 6)])
  
  ### what we need here from runfolds() output
  # [ ] pooled.obs.train
  # [ ] pooled.obs.test
  # [ ] pooled.pred.train
  # [ ] pooled.pred.test
  # [ ] currentRes (1 list)
  # [ ] currentImpo (1 list)
  
  # merge sublists of each fold into one list
  # http://stackoverflow.com/questions/32557131/adding-a-vector-to-each-sublist-within-a-list-r
  # http://stackoverflow.com/questions/43963683/r-flexible-passing-of-sublists-to-following-function
  runfolds_merged <- do.call(Map, c(f = list, runfolds_list))
  
  if (importance == TRUE) {
    # subset fold result to importance results only
    impo_only <- runfolds_merged[6][[1]]
    ### get mean from all impo results of all folds (multiple dataframes stored in a list)
    ### http://stackoverflow.com/questions/18371187/element-wise-mean-for-a-list-of-dataframes-with-na
    ### NICHT MITTELN, ENFACH ALLE IMPO (= FÜR JEDEN FOLD) ZURÜCKGEBEN
    # currentImpo <- Reduce("+", impo_only) / length(impo_only)
  }
  
  pooled_only <- runfolds_merged[c(1:4)] 
  pooled_only <- sapply(unique(names(pooled_only)), function(x) unname(unlist(pooled_only[names(pooled_only) == x])), simplify = FALSE)   
  
  # Jetzt die avg. werte pro repetition errechnen
  # Put the results from the pooled estimation into the pooled.err data structure:
  if (error.rep) {
    if (is.factor(data[, response])) {
      lev <- levels(data[, response])
      if (err.train) {
        pooled_only$pooled.obs.train <- factor(lev[pooled_only$pooled.obs.train], levels = lev)
      }
      pooled_only$pooled.obs.test <- factor(lev[pooled_only$pooled.obs.test], levels = lev)
      if (is.factor.prediction) {
        if (err.train) {
          pooled_only$pooled.pred.train <- factor(lev[pooled_only$pooled.pred.train], levels = lev) 
        } 
        pooled_only$pooled.pred.test <- factor(lev[pooled_only$pooled.pred.test], levels = lev) 
      } 
    }
    pooled.err.train <- NULL
    if (err.train) {
      pooled.err.train <- err.fun(pooled_only$pooled.obs.train, pooled_only$pooled.pred.train)
    }
    
    currentPooled.err <- t(unlist(list(train = pooled.err.train, test = err.fun(pooled_only$pooled.obs.test, 
                                                                                pooled_only$pooled.pred.test))))
    
    if (do.gc >= 2) {
      gc()
    }
  }  # end for each fold
  
  if ((do.gc >= 1) & (do.gc < 2)) {
    gc()
  }
  
  # set currentImpo to NULL to prevent false importance output (resamp object) 
  # if not desired 
  if (importance == FALSE) {
    impo_only <- NULL
  }
  
  #return(list(error = currentRes, pooled.error = currentPooled.err, importance = currentImpo))
  return(list(error = runfolds_merged$currentRes, pooled.error = currentPooled.err, importance = impo_only))
}


#' transfer_parallel_output
#' 
#' @keywords internal
#' @export
transfer_parallel_output <- function(myRes = NULL, res = NULL, impo = NULL,
                                     pooled.err = NULL) {
  
  for (i in seq_along(myRes)) {
    if (i == 1) {
      pooled.err <- myRes[[i]]$pooled.error
      impo[[i]] <- myRes[[i]]$importance
      res[[i]] <- myRes[[i]]$error
    } else {
      pooled.err <- rbind(pooled.err, myRes[[i]]$pooled.error)
      impo[[i]] <- myRes[[i]]$importance
      res[[i]] <- myRes[[i]]$error
    }
  }
  
  return(list(pooled.err = pooled.err, impo = impo,
              res = res))
}
