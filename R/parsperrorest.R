#' Perform spatial error estimation and variable importance assessment
#'
#' `parsperrorest` is a flexible interface for multiple types of 
#' parallelized spatial and non-spatial cross-validation 
#' and bootstrap error estimation and parallelized permutation-based 
#' assessment of spatial variable importance.
#' 
#' @inheritParams partition.cv
#' 
#' @import pbapply
#' @import rpart
#' @importFrom utils packageVersion 
#' @import snow
#' @importFrom parallel detectCores clusterSetRNGStream mclapply
#' @import foreach
#' @import doParallel
#' @import notifier
#' 
#' @param data a `data.frame` with predictor and response variables. 
#' Training and test samples will be drawn from this data set by `train.fun` 
#' and `test.fun`, respectively.
#' 
#' @param formula A formula specifying the variables used by the `model`. 
#' Only simple formulas without interactions or nonlinear terms should 
#' be used, e.g. `y~x1+x2+x3` but not `y~x1*x2+log(x3)`. 
#' Formulas involving interaction and nonlinear terms may possibly work 
#' for error estimation but not for variable importance assessment, 
#' but should be used with caution.
#' 
#' @param coords vector of length 2 defining the variables in `data` that 
#' contain the x and y coordinates of sample locations.
#' 
#' @param model.fun Function that fits a predictive model, such as `glm` 
#' or `rpart`. The function must accept at least two arguments, the first 
#' one being a formula and the second a data.frame with the learning sample.
#' @param model.args Arguments to be passed to `model.fun` 
#' (in addition to the `formula` and `data` argument, 
#' which are provided by `sperrorest`)
#' 
#' @param pred.fun Prediction function for a fitted model object created 
#' by `model`. Must accept at least two arguments: the fitted 
#' `object` and a `data.frame` `newdata` with data 
#' on which to predict the outcome.
#' 
#' @param pred.args (optional) Arguments to `pred.fun` (in addition to the 
#' fitted model object and the `newdata` argument, 
#' which are provided by `sperrorest`)
#' 
#' @param smp.fun A function for sampling training and test sets from 
#' `data`. E.g., [partition.kmeans()] for 
#' spatial cross-validation using spatial \emph{k}-means clustering.
#' 
#' @param smp.args (optional) Arguments to be passed to `est.fun`
#' 
#' @param train.fun (optional) A function for resampling or subsampling the 
#' training sample in order to achieve, e.g., uniform sample sizes on all 
#' training sets, or maintaining a certain ratio of positives and negatives 
#' in training sets. 
#' E.g., [resample.uniform()] or [resample.strat.uniform()]
#' 
#' @param train.param (optional) Arguments to be passed to `resample.fun`
#' 
#' @param test.fun (optional) Like `train.fun` but for the test set.
#' 
#' @param test.param (optional) Arguments to be passed to `test.fun`
#' 
#' @param err.fun A function that calculates selected error measures from the 
#' known responses in `data` and the model predictions delivered 
#' by `pred.fun`. E.g., [err.default()] (the default). 
#' See example and details below.
#' 
#' @param error.fold logical (default: `TRUE`) if `importance` is 
#' `TRUE`, otherwise `FALSE`): calculate error measures on each fold 
#' within a resampling repetition.
#' 
#' @param error.rep logical (default: `TRUE`): calculate error measures 
#' based on the pooled predictions of all folds within a resampling repetition.
#' 
#' @param err.train logical (default: `TRUE`): calculate error measures on 
#' the training set (in addition to the test set estimation).
#' 
#' @param imp.variables (optional; used if `importance = TRUE`) 
#' Variables for which permutation-based variable importance assessment 
#' is performed. If `importance = TRUE` and `imp.variables` is 
#' `NULL`, all variables in `formula` will be used.
#' 
#' @param imp.permutations (optional; used if `importance = TRUE`) 
#' Number of permutations used for variable importance assessment.
#' 
#' @param importance logical: perform permutation-based variable 
#' importance assessment?
#' 
#' @param ... currently not used
#' 
#' @param distance logical (default: `FALSE`): if `TRUE`, calculate 
#' mean nearest-neighbour distances from test samples to training samples using 
#' [add.distance.represampling()]
#' 
#' @param do.gc numeric (default: 1): defines frequency of memory garbage 
#' collection by calling [gc()]; if `<1`, no garbage collection; 
#' if `>=1`, run a `gc()` after each repetition; 
#' if `>=2`, after each fold
#' 
#' @param do.try logical (default: `FALSE`): if `TRUE` (untested!!), 
#' use [try()] to robustify calls to `model.fun` and 
#' `err.fun`; use with caution!
#' 
#' @param progress numeric (default: `1`): Whether to show progress 
#' information. For `par.mode = 1`, information about elapsed time, estimated time remaining and a 
#' percentage indicator (0\% - 100\%) are shown. 
#' `progress = 2` only applies to `par.mode = 2` and shows repetition 
#' information only (instead of repetition and fold).
#' Set to `FALSE` for no progress information. 
#' 
#' @param out.progress only used if `par.mode = 2`: Optionally write progress output to a file instead of console output. 
#' The default (`''`) results in console output for Unix-systems and
#' file output ('parsperrorest.progress.txt') in the current working directory 
#' for Windows-systems. 
#' 
#' @param notify (optional) show a notification badge after `parsperrorest()` has finished.
#' 
#' @param par.args list of parallelization parameters:
#' `par.mode` (the parallelization mode),
#' `par.units` (the number of parallel processing units), 
#' `par.libs` (libraries to be loaded on cluster workers, character list).
#' See Details for more information.
#' 
#' @param benchmark (optional) logical (default: `FALSE`): if `TRUE`, 
#' perform benchmarking and return `sperrorestbenchmarks` object
#' 
#' @return A list (object of class `sperrorest`) with (up to) six components:
#' \item{error.rep}{a `sperrorestreperror` object containing 
#' predictive performances at the repetition level}
#' \item{error.fold}{a `sperroresterror` object containing predictive 
#' performances at the fold level}
#' \item{represampling}{a [represampling()] object}
#' \item{importance}{a `sperrorestimportance` object containing 
#' permutation-based variable importances at the fold level}
#' \item{benchmarks}{a `sperrorestbenchmarks` object containing 
#' information on the system the code is running on, starting and 
#' finishing times, number of available CPU cores, parallelization mode, 
#' number of parallel units, and runtime performance}
#' \item{package.version}{a `sperrorestpackageversion` object containing 
#' information about the `sperrorest` package version}
#' 
#' @details Two `par.mode` options are availabe. The default mode is 
#' `par.mode = 1`. Here, [pbapply::pblapply()] is used which 
#' either calls [parallel::mclapply()] (on Unix-systems) or 
#' [parallel::parApply()] (on Windows-systems). `par.mode = 2`
#' uses [foreach::foreach()]. While this approach is not as efficient,
#' it may work in cases in which `par.mode = 1` fails.
#' 
#' @details `par.libs` only applies to `par.mode = 1` on Windows-systems.
#' 
#' @details This parallelized version of [sperrorest()] may highly 
#' decrease computation time. However, please note that problems
#' may occur depending on which function is used for cross-validation. 
#' While the [rpart::rpart()] example (see Examples) here works fine, you may 
#' encounter problems with other functions. 
#' 
#' @details For `par.mode = 2`, you may encounter missing repetitions in the results
#' if repetitions finish to quickly. In this case, consider using 
#' [sperrorest()]
#' 
#' @details Known problems when being parallized: [randomForest::randomForest()]
#' 
#' @details If you define a custom `pred.fun` which conists of multiple custom 
#' defined child functions, make sure to define `pred.fun` and all child 
#' functions in one call. Otherwise you will encounter errors in `par.mode = 2`
#' caused by how `foreach` loads the parent environment.
#' 
#' 
#' @note (1) Optionally save fitted models, training and test samples in the 
#' results object; (2) Optionally save intermediate results in some file, and 
#' enable the function to continue an interrupted sperrorest call where it 
#' was interrupted. (3) Optionally have sperrorest dump the result of each 
#' repetition into a file, and to skip repetitions for which a file already exists. 
#' 
#' @references Brenning, A. 2012. Spatial cross-validation and bootstrap for 
#' the assessment of prediction rules in remote sensing: the R package 'sperrorest'. 
#' 2012 IEEE International Geoscience and Remote Sensing Symposium (IGARSS), 
#' 23-27 July 2012, p. 5372-5375.
#'
#' Brenning, A. 2005. Spatial prediction models for landslide hazards: review, 
#' comparison and evaluation. Natural Hazards and Earth System Sciences, 5(6): 853-862.
#'
#' Brenning, A., S. Long & P. Fieguth. Forthcoming. Detecting rock glacier flow 
#' structures using Gabor filters and IKONOS imagery. 
#' Submitted to Remote Sensing of Environment.
#'
#' Russ, G. & A. Brenning. 2010a. Data mining in precision agriculture: 
#' Management of spatial information. In 13th International Conference on 
#' Information Processing and Management of Uncertainty, IPMU 2010; Dortmund; 
#' 28 June - 2 July 2010. Lecture Notes in Computer Science, 6178 LNAI: 350-359.
#'
#' Russ, G. & A. Brenning. 2010b. Spatial variable importance assessment for 
#' yield prediction in Precision Agriculture. In Advances in Intelligent 
#' Data Analysis IX, Proceedings, 9th International Symposium, 
#' IDA 2010, Tucson, AZ, USA, 19-21 May 2010.  
#' Lecture Notes in Computer Science, 6065 LNCS: 184-195.
#' 
#' @seealso [sperrorest()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' 
#' # Example of a classification tree fitted to this data:
#' library(rpart)
#' mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
#' ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
#' fit <- rpart(fo, data = ecuador, control = ctrl)
#'
#' # Non-spatial 5-repeated 10-fold cross-validation:
#' mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
#' par.nsp.res <- parsperrorest(data = ecuador, formula = fo,
#'                              model.fun = rpart, model.args = list(control = ctrl),
#'                              pred.fun = mypred.rpart,
#'                              progress = TRUE,
#'                              smp.fun = partition.cv, 
#'                              smp.args = list(repetition = 1:5, nfold = 15), 
#'                              par.args = list(par.units = 2, par.mode = 1),
#'                              error.rep = TRUE, error.fold = TRUE)
#' summary(par.nsp.res$error.rep)
#' summary(par.nsp.res$error.fold)
#' summary(par.nsp.res$represampling)
#' # plot(par.nsp.res$represampling, ecuador)
#'
#' # Spatial 5-repeated 10-fold spatial cross-validation:
#' par.sp.res <- parsperrorest(data = ecuador, formula = fo,
#'                             model.fun = rpart, model.args = list(control = ctrl),
#'                             pred.fun = mypred.rpart,
#'                             progress = TRUE,
#'                             smp.fun = partition.kmeans, 
#'                             smp.args = list(repetition = 1:5, nfold = 15), 
#'                             par.args = list(par.units = 2, par.mode = 2),
#'                             error.rep = TRUE, error.fold = TRUE)
#' summary(par.sp.res$error.rep)
#' summary(par.sp.res$error.fold)
#' summary(par.sp.res$represampling)
#' # plot(par.sp.res$represampling, ecuador)
#' 
#' smry <- data.frame(
#'     nonspat.training = unlist(summary(par.nsp.res$error.rep, level = 1)$train.auroc),
#'     nonspat.test     = unlist(summary(par.nsp.res$error.rep, level = 1)$test.auroc),
#'     spatial.training = unlist(summary(par.sp.res$error.rep, level = 1)$train.auroc),
#'     spatial.test     = unlist(summary(par.sp.res$error.rep, level = 1)$test.auroc))
#' boxplot(smry, col = c('red','red','red','green'), 
#'     main = 'Training vs. test, nonspatial vs. spatial',
#'     ylab = 'Area under the ROC curve')
#'     
#' @export
parsperrorest <- function(formula, data, coords = c("x", "y"), model.fun, model.args = list(), 
  pred.fun = NULL, pred.args = list(), smp.fun = partition.loo, smp.args = list(), 
  train.fun = NULL, train.param = NULL, test.fun = NULL, test.param = NULL, err.fun = err.default, 
  error.fold = TRUE, error.rep = TRUE, err.train = TRUE, imp.variables = NULL, 
  imp.permutations = 1000, importance = !is.null(imp.variables), distance = FALSE, 
  do.gc = 1, do.try = FALSE, progress = 1, out.progress = "", notify = FALSE, 
  par.args = list(), benchmark = FALSE, ...)
  {
  # if benchmark = TRUE, start clock
  if (benchmark) 
    start.time <- Sys.time()
  
  # Some checks:
  if (missing(model.fun)) 
    stop("'model.fun' is a required argument")
  if (as.character(attr(terms(formula), "variables"))[3] == "...") 
    stop("formula of the form lhs ~ ... not accepted by 'sperrorest'\n
     specify all predictor variables explicitly")
  stopifnot(is.function(model.fun))
  stopifnot(is.function(smp.fun))
  if (!is.null(train.fun)) 
    stopifnot(is.function(train.fun))
  if (!is.null(test.fun)) 
    stopifnot(is.function(test.fun))
  stopifnot(is.function(err.fun))
  if (importance)
  {
    if (!error.fold)
    {
      warning("'importance=TRUE' currently only supported with 
        'error.fold=TRUE'.\nUsing 'importance=FALSE'")
      importance <- FALSE
    }
    stopifnot(is.numeric(imp.permutations))
    if (!is.null(imp.variables)) 
      stopifnot(is.character(imp.variables))
  }
  stopifnot(is.character(coords))
  stopifnot(length(coords) == 2)
  if (importance & !error.fold) 
    stop("variable importance assessment currently only supported 
     at the unpooled level")
  
  # Check if user is trying to bypass the normal mechanism for generating training
  # and test data sets and for passing formulas:
  if (any(names(model.args) == "formula")) 
    stop("'model.args' cannot have a 'formula' element")
  if (any(names(model.args) == "data")) 
    stop("'model.args' cannot have a 'data' element")
  if (any(names(pred.args) == "object")) 
    stop("'pred.args' cannot have an 'object' element:\n
     this will be generated by 'sperrorest'")
  if (any(names(pred.args) == "newdata")) 
    stop("'pred.args' cannot have a 'newdata' element:\n
     this will be generated by 'sperrorest'")
  
  # Some checks related to recent changes in argument names:
  dots.args <- list(...)
  if (length(dots.args) > 0)
  {
    if (any(names(dots.args) == "predfun"))
    {
      stop("sorry: argument names have changed; 'predfun' is now 'pred.fun'")
    }
    if (any(names(dots.args) == "silent"))
    {
      stop("sorry: argument names have changed; 'silent' is now 'progress'")
    }
    if (any(names(dots.args) == "err.pooled"))
    {
      stop("sorry: argument names have changed; 'err.pooled' is now 'error.rep'")
    }
    if (any(names(dots.args) == "err.unpooled"))
    {
      stop("sorry: argument names have changed; 'err.unpooled' is now 'error.fold'")
    }
    warning("'...' arguments currently not supported:\n
      use 'model.args' to pass list of additional 
            arguments to 'model.fun'")
  }
  
  # Name of response variable:
  response <- as.character(attr(terms(formula), "variables"))[2]
  
  smp.args$data <- data
  smp.args$coords <- coords
  
  resamp <- do.call(smp.fun, args = smp.args)
  if (distance) 
    # Parallelize this function???
  resamp <- add.distance(resamp, data, coords = coords, fun = mean)
  
  if (error.fold)
  {
    res <- lapply(resamp, unclass)
    class(res) <- "sperroresterror"
  } else res <- NULL
  pooled.err <- NULL
  # required to be able to assign levels to predictions if appropriate:
  is.factor.prediction <- NULL
  
  ### Permutation-based variable importance assessment (optional):
  impo <- NULL
  if (importance)
  {
    # Importance of which variables:
    if (is.null(imp.variables)) 
      imp.variables <- strsplit(as.character(formula)[3], " + ", fixed = TRUE)[[1]]
    # Dummy data structure that will later be populated with the results:
    impo <- resamp
    # Create a template that will contain results of variable importance assessment:
    imp.one.rep <- as.list(rep(NA, length(imp.variables)))
    names(imp.one.rep) <- imp.variables
    tmp <- as.list(rep(NA, imp.permutations))
    names(tmp) <- as.character(1:imp.permutations)
    for (vnm in imp.variables) imp.one.rep[[vnm]] <- tmp
    rm(tmp)
  }
  # par.mode = 1 (pbapply) -------
  if (par.args$par.mode == 1)
  {
    
    # runreps function for lapply()
    runreps <- function(currentSample)
    {
      # output data structures
      currentRes <- NULL
      currentImpo <- currentSample
      currentPooled.err <- NULL
      
      if (error.fold)
      {
        currentRes <- lapply(currentSample, unclass)
        class(currentRes) <- "sperroresterror"
      } else currentRes <- NULL
      
      # Collect pooled results in these data structures:
      if (err.train) 
        pooled.obs.train <- pooled.pred.train <- c()
      pooled.obs.test <- pooled.pred.test <- c()
      
      # Parallelize this???  For each fold:
      for (j in 1:length(currentSample))
      {
        
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
        
        if (err.train)
        {
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
          if (error.fold) 
          if (do.try)
          {
            err.try <- try(err.fun(nd[, response], pred.train))
            if (class(err.try) == "try-error") 
            err.try <- NULL
            currentRes[[j]]$train <- err.try  #res[[i]][[j]]$train = err.try
          } else
          {
            currentRes[[j]]$train <- err.fun(nd[, response], pred.train)  #res[[i]][[j]]$train = err.fun(nd[,response], pred.train)
          }
          if (error.rep)
          {
          pooled.obs.train <- c(pooled.obs.train, nd[, response])
          pooled.pred.train <- c(pooled.pred.train, pred.train)
          }
        } else
        {
          if (error.fold) 
          currentRes[[j]]$train <- NULL  #res[[i]][[j]]$train = NULL
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
          is.factor.prediction <- is.factor(pred.test)
        }
        
        ### Permutation-based variable importance assessment:
        if (importance & error.fold)
        {
          
          if (is.null(currentRes[[j]]$test)) 
          {
            currentImpo[[j]] <- c()
            
            # Parallelize this: ???
            for (cnt in 1:imp.permutations)
            {
            
            # Permutation indices:
            permut <- sample(1:nrow(nd), replace = FALSE)
            
            # For each variable:
            for (vnm in imp.variables)
            {
              # Get undisturbed backup copy of test sample:
              nd <- nd.bak
              # Permute variable vnm:
              nd[, vnm] <- nd[, vnm][permut]
              # Apply model to perturbed test sample:
              pargs <- c(list(object = fit, newdata = nd), pred.args)
              if (is.null(pred.fun))
              {
              pred.test <- do.call(predict, args = pargs)
              } else
              {
              pred.test <- do.call(pred.fun, args = pargs)
              }
              rm(pargs)
              
              # Calculate variable importance:
              if (do.try)
              {
              permut.err <- try(err.fun(nd[, response], pred.test))
              if (class(permut.err) == "try-error")
              {
                imp.temp[[vnm]][[cnt]] <- c()  # ???
              } else
              {
                imp.temp[[vnm]][[cnt]] <- as.list(unlist(currentRes[[j]]$test) - 
                unlist(permut.err))
                # as.list( unlist(res[[i]][[j]]$test) - unlist(permut.err) ) (apply '-' to
                # corresponding list elements; only works if all list elements are scalars)
              }
              } else
              {
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
        
      }
      
      # Put the results from the pooled estimation into the pooled.err data structure:
      if (error.rep) 
        {
          if (is.factor(data[, response]))
          {
          lev <- levels(data[, response])
          if (err.train) 
            pooled.obs.train <- factor(lev[pooled.obs.train], levels = lev)
          pooled.obs.test <- factor(lev[pooled.obs.test], levels = lev)
          if (is.factor.prediction)
          {
            if (err.train) 
            pooled.pred.train <- factor(lev[pooled.pred.train], levels = lev)
            pooled.pred.test <- factor(lev[pooled.pred.test], levels = lev)
          }
          }
          pooled.err.train <- NULL
          if (err.train) 
          pooled.err.train <- err.fun(pooled.obs.train, pooled.pred.train)
          
          currentPooled.err <- t(unlist(list(train = pooled.err.train, test = err.fun(pooled.obs.test, 
          pooled.pred.test))))
          
          if (do.gc >= 2) 
          gc()
        }  # end for each fold
      
      if ((do.gc >= 1) & (do.gc < 2)) 
        gc()
      return(list(error = currentRes, pooled.error = currentPooled.err, importance = currentImpo))
    }
    
    if (par.args$par.units > detectCores())
    {
      par.args$par.units <- detectCores()
    }
    
    
    # parallelization here (par.mode = 1 & par.mode = 2) For each repetition:
    if (.Platform$OS.type == "windows")
    {
      par.cl <- makeCluster(par.args$par.units, type = "SOCK")
      clusterSetRNGStream(par.cl, 1234567)  #set up RNG stream to obtain 
      # reproducible results
      force(pred.fun)  #force evaluation of pred.fun, so it is serialized and 
      # provided to all cluster workers
      clusterExport(par.cl, "par.args", envir = environment())
      clusterEvalQ(par.cl, {
        lapply(X = par.args$par.libs, FUN = function(n)
        {
          do.call("library", list(n))
        })
        NULL
      })
    } else
    {
      RNGkind("L'Ecuyer-CMRG")
      set.seed(1234567)
      # mc.reset.stream() #set up RNG stream to obtain reproducible results # comment:
      # causing build to fail on Windows. Not sure if really necessary -> not in use
      # for now
      par.cl <- par.args$par.units
    }
    
    # parLapply is called when cl is a 'cluster' object, mclapply is called when cl
    # is an integer. Showing the progress bar increases the communication overhead
    # between the main process and nodes / child processes compared to the parallel
    # equivalents of the functions without the progress bar.  see ?pblapply
    pboptions(style = 1, type = "timer")
    myRes <- pblapply(cl = par.cl, X = resamp, FUN = runreps)
    
    if (par.args$par.mode == 2)
    {
      stopCluster(par.cl)
    }
    
    
    
    # transfer results of lapply() to respective data objects
    for (i in 1:length(myRes))
    {
      if (i == 1)
      {
        pooled.err <- myRes[[i]]$pooled.error
        impo[[i]] <- myRes[[i]]$importance
        res[[i]] <- myRes[[i]]$error
      } else
      {
        pooled.err <- rbind(pooled.err, myRes[[i]]$pooled.error)
        impo[[i]] <- myRes[[i]]$importance
        res[[i]] <- myRes[[i]]$error
      }
    }
    
    
    # convert matrix(?) to data.frame:
    if (error.rep)
    {
      pooled.err <- as.data.frame(pooled.err)
      rownames(pooled.err) <- NULL
      class(pooled.err) <- "sperrorestreperror"
    }
    
    if (importance) 
      class(impo) <- "sperrorestimportance"
    
    if (benchmark)
    {
      end.time <- Sys.time()
      my.bench <- list(system.info = Sys.info(), t.start = start.time, t.end = end.time, 
        cpu.cores = detectCores(), par.mode = par.args$par.mode, par.units = par.args$par.units, 
        runtime.performance = end.time - start.time)
      class(my.bench) <- "sperrorestbenchmarks"
    } else my.bench <- NULL
    
    
    if (notify == TRUE) {
      if (benchmark == TRUE) {
        msg <- paste0("Repetitions: ", length(smp.args$repetition), "; ", 
                      "Folds: ", smp.args$nfold, "; ", "Total time: ", round(my.bench$runtime.performance, 
                                                                             2))
      } else (msg <- paste0("Repetitions: ", length(smp.args$repetition), "; ", 
                            "Folds: ", smp.args$nfold))
      
      notify(title = "parsperrorest() finished successfully!", msg <- msg)
    }
    
    package.version <- packageVersion("sperrorest")
    class(package.version) <- "sperrorestpackageversion"
    
    RES <- list(error.rep = pooled.err, error.fold = res, represampling = resamp, 
      importance = impo, benchmarks = my.bench, package.version = package.version)
    class(RES) <- "sperrorest"
    
    return(RES)
    
  }
  # par.mode = 2 (foreach) -------
  if (par.args$par.mode == 2)
  {
    
    
    # combine function for multiple object outputs in foreach call
    comb <- function(...)
    {
      mapply("rbind", ..., SIMPLIFY = FALSE)
    }
    
    # suppress any progress output of workes if progress = FALSE
    if (progress == FALSE)
    {
      out.progress <- "/dev/null"
      if (Sys.info()["sysname"] == "Windows")
      {
        out.progress <- "nul:"
      }
    }
    # special settings for Windows
    if (out.progress == "" & Sys.info()["sysname"] == "Windows") 
      out.progress <- paste0(getwd(), "/parsperrorest.progress.txt")
    
    cl <- makeCluster(par.args$par.units, outfile = out.progress)
    registerDoParallel(cl)
    
    foreach.out <- foreach(i = 1:length(resamp), .packages = (.packages()), .errorhandling = "remove", 
      .combine = "comb", .multicombine = TRUE, .verbose = FALSE) %dopar% {
      
      # reset rep.err otherwise duplicates are introduced
      rep.err <- NULL
      
      if (err.train)
      {
        pooled.obs.train <- pooled.pred.train <- c()
        pooled.obs.test <- pooled.pred.test <- c()
      }
      for (j in 1:length(resamp[[i]]))
      {
        if (progress == TRUE | progress == 1)
        {
          cat(date(), "Repetition", names(resamp)[i], "- Fold", j, "\n")
        }
        if (progress == 2)
        {
          cat(date(), "Repetition", names(resamp)[i], "\n")
        }
        nd <- data[resamp[[i]][[j]]$train, ]
        if (!is.null(train.fun))
        {
          nd <- train.fun(data = nd, param = train.param)
        }
        margs <- c(list(formula = formula, data = nd), model.args)
        if (do.try)
        {
          fit <- try(do.call(model.fun, args = margs))
          if (class(fit) == "try-error")
          {
          fit <- NULL
          if (error.fold)
          {
            if (err.train)
            {
            res[[i]][[j]]$train <- NULL
            }
            res[[i]][[j]]$test <- NULL
            if (importance)
            {
            impo[[i]][[j]] <- c()
            }
          }
          if (do.gc >= 2)
          {
            gc()
          }
          next
          }
        } else
        {
          fit <- do.call(model.fun, args = margs)
        }
        if (err.train)
        {
          pargs <- c(list(object = fit, newdata = nd), pred.args)
          if (is.null(pred.fun))
          {
          pred.train <- do.call(predict, args = pargs)
          } else
          {
          pred.train <- do.call(pred.fun, args = pargs)
          }
          rm(pargs)
          if (error.fold) 
          if (do.try)
          {
            err.try <- try(err.fun(nd[, response], pred.train))
            if (class(err.try) == "try-error") 
            err.try <- NULL
            res[[i]][[j]]$train <- err.try
          } else
          {
            res[[i]][[j]]$train <- err.fun(nd[, response], pred.train)
          }
          if (error.rep)
          {
          pooled.obs.train <- c(pooled.obs.train, nd[, response])
          pooled.pred.train <- c(pooled.pred.train, pred.train)
          }
        } else
        {
          if (error.fold)
          {
          res[[i]][[j]]$train <- NULL
          }
        }
        nd <- data[resamp[[i]][[j]]$test, ]
        if (!is.null(test.fun))
        {
          nd <- test.fun(data = nd, param = test.param)
        }
        if (importance)
        {
          nd.bak <- nd
        }
        pargs <- c(list(object = fit, newdata = nd), pred.args)
        if (is.null(pred.fun))
        {
          pred.test <- do.call(predict, args = pargs)
        } else
        {
          pred.test <- do.call(pred.fun, args = pargs)
        }
        rm(pargs)
        if (error.fold)
        {
          if (do.try)
          {
          err.try <- try(err.fun(nd[, response], pred.test))
          if (class(err.try) == "try-error") 
            err.try <- NULL
          res[[i]][[j]]$test <- err.try
          } else
          {
          res[[i]][[j]]$test <- err.fun(nd[, response], pred.test)
          }
        }
        if (error.rep)
        {
          pooled.obs.test <- c(pooled.obs.test, nd[, response])
          pooled.pred.test <- c(pooled.pred.test, pred.test)
          is.factor.prediction <- is.factor(pred.test)
        }
        if (importance & error.fold)
        {
          if (is.null(res[[i]][[j]]$test))
          {
          impo[[i]][[j]] <- c()
          if (!progress == FALSE) 
            cat(date(), "-- skipping variable importance\n")
          } else
          {
          if (!progress == FALSE)
          {
            cat(date(), "-- Variable importance\n")
          }
          imp.temp <- imp.one.rep
          for (cnt in 1:imp.permutations)
          {
            if (!progress == FALSE & (cnt > 1)) 
            if (log10(cnt) == floor(log10(cnt))) 
              cat(date(), "   ", cnt, "\n")
            permut <- sample(1:nrow(nd), replace = FALSE)
            for (vnm in imp.variables)
            {
            nd <- nd.bak
            nd[, vnm] <- nd[, vnm][permut]
            pargs <- c(list(object = fit, newdata = nd), pred.args)
            if (is.null(pred.fun))
            {
              pred.test <- do.call(predict, args = pargs)
            } else
            {
              pred.test <- do.call(pred.fun, args = pargs)
            }
            rm(pargs)
            if (do.try)
            {
              permut.err <- try(err.fun(nd[, response], pred.test))
              if (class(permut.err) == "try-error")
              {
              imp.temp[[vnm]][[cnt]] <- c()
              } else
              {
              imp.temp[[vnm]][[cnt]] <- as.list(unlist(res[[i]][[j]]$test) - 
                unlist(permut.err))
              }
            } else
            {
              permut.err <- err.fun(nd[, response], pred.test)
              imp.temp[[vnm]][[cnt]] <- as.list(unlist(res[[i]][[j]]$test) - 
              unlist(permut.err))
            }
            }
          }
          impo[[i]][[j]] <- as.data.frame(t(sapply(imp.temp, function(y) sapply(as.data.frame(t(sapply(y, 
            as.data.frame))), function(x) mean(unlist(x))))))
          rm(nd.bak, nd)
          }
        }
        # res <- res[[i]][1, ]
      }  #end of each fold
      # res[[i]] <- res[[i]][1, ]
      if (error.rep)
      {
        if (is.factor(data[, response]))
        {
          lev <- levels(data[, response])
          if (err.train)
          {
          pooled.obs.train <- factor(lev[pooled.obs.train], levels = lev)
          pooled.obs.test <- factor(lev[pooled.obs.test], levels = lev)
          }
          if (is.factor.prediction)
          {
          if (err.train)
          {
            pooled.pred.train <- factor(lev[pooled.pred.train], levels = lev)
            pooled.pred.test <- factor(lev[pooled.pred.test], levels = lev)
          }
          }
          rep.err.train <- NULL
          if (err.train)
          {
          rep.err.train <- err.fun(pooled.obs.train, pooled.pred.train)
          }
          if (i == 1)
          {
          rep.err <- t(unlist(list(train = rep.err.train, test = err.fun(pooled.obs.test, 
            pooled.pred.test))))
          } else
          {
          rep.err <- rbind(rep.err, unlist(list(train = rep.err.train, 
            test = err.fun(pooled.obs.test, pooled.pred.test))))
          }
          if (do.gc >= 2)
          {
          gc()
          }
        }
        if ((do.gc >= 1) & (do.gc < 2))
        {
          gc()
        }
      }
      
      if (error.rep & error.fold)
      {
        
        foreach.out <- list(rep.err, res)
        foreach.out <- list(foreach.out)
        return(foreach.out)
      }
      if (error.rep & !error.fold)
      {
        foreach.out <- rep.err
        return(foreach.out)
      }
      if (!error.rep & error.fold)
      {
        foreach.out <- res
        return(foreach.out)
      }
    }
    stopCluster(cl)
    
    # end foreach() ------
    if (error.rep & !error.fold)
    {
      rep.err <- as.data.frame(foreach.out)
    }
    if (error.rep & error.fold)
    {
      
      ## error.rep output as matrix -> conver to dataframe and merge all repetitions
      ## convert matrix to data.frame (error.rep) hier nur länge reps!! (toDo)
      for (i in 1:length(resamp))
      {
        foreach.out[[1]][[i]] <- as.data.frame(foreach.out[[1]][[i]])
      }
      # merge all reps into one data.frame
      for (i in 2:length(resamp))
      {
        foreach.out[[1]][[1]] <- merge.data.frame(foreach.out[[1]][[1]], 
          foreach.out[[1]][[i]], all = TRUE)
      }
      # remove still existing single rep data.frames
      i <- 2
      while (i <= length(resamp))
      {
        foreach.out[[1]][[2]] <- NULL
        i <- i + 1
      }
      
      foreach.out.tmp <- foreach.out[[1]]
      foreach.out.tmp[[1]] <- NULL
      for (i in seq_along(resamp))
      {
        foreach.out.tmp[[i]] <- foreach.out.tmp[[i]][[i]]
      }
      err.fold <- foreach.out.tmp
    }
    
    if (!error.rep & error.fold)
    {
      # multiple (unnecessary) lists are written in foreach loop. Reason unknown.
      # Subset to important lists only containing fold error measures
      for (i in seq_along(resamp))
      {
        foreach.out[[i]] <- foreach.out[[i]][i, ]
      }
    }
    
    if (!progress == FALSE)
    {
      cat(date(), "Done.\n")
    }
    if (importance)
    {
      class(impo) <- "sperrorestimportance"
    }
    
    if (benchmark)
    {
      end.time <- Sys.time()
      my.bench <- list(system.info = Sys.info(), t.start = start.time, t.end = end.time, 
        cpu.cores = detectCores(), par.mode = par.args$par.mode, par.units = par.args$par.units, 
        runtime.performance = end.time - start.time)
      class(my.bench) <- "sperrorestbenchmarks"
    } else my.bench <- NULL
    
    if (notify == TRUE) {
      if (benchmark == TRUE) {
        msg <- paste0("Repetitions: ", length(smp.args$repetition), "; ", 
                      "Folds: ", smp.args$nfold, "; ", "Total time: ", round(my.bench$runtime.performance, 
                                                                             2))
      } else (msg <- paste0("Repetitions: ", length(smp.args$repetition), "; ", 
                            "Folds: ", smp.args$nfold))
      
      notify(title = "parsperrorest() finished successfully!", msg <- msg)
    }
    
    if (error.rep & error.fold)
    {
      class(err.fold) <- "sperroresterror"
      package.version <- packageVersion("sperrorest")
      class(package.version) <- "sperrorestpackageversion"
      # this 'class' converts from data.frame to list (sperrorestreperror)
      class(foreach.out[[1]][[1]]) <- "sperrorestreperror"
      RES <- list(error.rep = foreach.out[[1]][[1]], error.fold = err.fold, 
        represampling = resamp, importance = impo, benchmarks = my.bench, 
        package.version = package.version)
      class(RES) <- "sperrorest"
      return(RES)
    }
    if (error.rep & !error.fold)
    {
      class(rep.err) <- "sperrorestreperror"
      package.version <- packageVersion("sperrorest")
      class(package.version) <- "sperrorestpackageversion"
      RES <- list(error.rep = rep.err, error.fold = NULL, represampling = resamp, 
        importance = impo, benchmarks = my.bench, package.version = package.version)
      class(RES) <- "sperrorest"
      return(RES)
    }
    if (!error.rep & error.fold)
    {
      class(foreach.out) <- "sperroresterror"
      package.version <- packageVersion("sperrorest")
      class(package.version) <- "sperrorestpackageversion"
      RES <- list(error.rep = NULL, error.fold = foreach.out, represampling = resamp, 
        importance = impo, benchmarks = my.bench, package.version = package.version)
      class(RES) <- "sperrorest"
      return(RES)
    }
  }
  
}

