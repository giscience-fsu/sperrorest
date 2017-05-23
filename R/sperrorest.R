#' Perform spatial error estimation and variable importance assessment in parallel
#'
#' `sperrorest` is a flexible interface for multiple types of 
#' parallelized spatial and non-spatial cross-validation 
#' and bootstrap error estimation and parallelized permutation-based 
#' assessment of spatial variable importance.
#' 
#' @inheritParams partition.cv
#' 
#' @import pbapply
#' @import magrittr
#' @import pbmcapply
#' @import parallel
#' @importFrom foreach %dopar% foreach
#' @import doFuture
#' @import rpart
#' @importFrom utils packageVersion 
#' @import future
#' @importFrom purrr walk map
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
#' `data`. E.g., [partition.kmeans] for 
#' spatial cross-validation using spatial \emph{k}-means clustering.
#' 
#' @param smp.args (optional) Arguments to be passed to `smp.fun`
#' 
#' @param train.fun (optional) A function for resampling or subsampling the 
#' training sample in order to achieve, e.g., uniform sample sizes on all 
#' training sets, or maintaining a certain ratio of positives and negatives 
#' in training sets. 
#' E.g., [resample.uniform] or [resample.strat.uniform]
#' 
#' @param train.param (optional) Arguments to be passed to `resample.fun`
#' 
#' @param test.fun (optional) Like `train.fun` but for the test set.
#' 
#' @param test.param (optional) Arguments to be passed to `test.fun`
#' 
#' @param err.fun A function that calculates selected error measures from the 
#' known responses in `data` and the model predictions delivered 
#' by `pred.fun`. E.g., [err.default] (the default). 
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
#' [add.distance.represampling]
#' 
#' @param do.gc numeric (default: 1): defines frequency of memory garbage 
#' collection by calling [gc]; if `<1`, no garbage collection; 
#' if `>=1`, run a [gc] after each repetition; 
#' if `>=2`, after each fold
#' 
#' @param do.try logical (default: `FALSE`): if `TRUE` (untested!!), 
#' use [try] to robustify calls to `model.fun` and 
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
#' file output ('sperrorest.progress.txt') in the current working directory 
#' for Windows-systems. 
#' 
#' @param par.args list of parallelization parameters:
#' `par.mode` (the parallelization mode),
#' `par.units` (the number of parallel processing units), 
#' `par.option` (optional settings for `par.mode = "future"`),
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
#' @details By default `sperrorest` runs in parallel on all cores using `foreach`
#' with the [future] backend. If this is not desired, specify 
#' `par.units` in `par.args`.
#' 
#' Other parallelization modes include `apply` ([pbmclapply] on Unix, [parApply]
#' on Windows) and `future` ([future_lapply]). For the latter `par.options` (default to
#' `multiprocess`) can be specified. See [plan] for further details. 
#' 
#' Setting `par.mode = "sequential"` executes `sperrorest` like a normal for-loop.
#' 
#' @note Custom predict functions passed to `pred.fun`, which consist of multiple custom 
#' defined child functions, must be defined in one function.  
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
#' 
#' @examples
#' \dontrun{
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
#' par.nsp.res <- sperrorest(data = ecuador, formula = fo,
#'                           model.fun = rpart, model.args = list(control = ctrl),
#'                           pred.fun = mypred.rpart,
#'                           progress = TRUE,
#'                           smp.fun = partition.cv, 
#'                           smp.args = list(repetition = 1:5, nfold = 10), 
#'                           error.rep = TRUE, error.fold = TRUE)
#' summary(par.nsp.res$error.rep)
#' summary(par.nsp.res$error.fold)
#' summary(par.nsp.res$represampling)
#' # plot(par.nsp.res$represampling, ecuador)
#'
#' # Spatial 5-repeated 10-fold spatial cross-validation:
#' par.sp.res <- sperrorest(data = ecuador, formula = fo,
#'                          model.fun = rpart, model.args = list(control = ctrl),
#'                          pred.fun = mypred.rpart,
#'                          progress = TRUE,
#'                          smp.fun = partition.kmeans, 
#'                          smp.args = list(repetition = 1:5, nfold = 10), 
#'                          par.args = list(par.units = 2, par.mode = "future"),
#'                          error.rep = TRUE, error.fold = TRUE)
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
#' }    
#' @export
sperrorest <- function(formula, data, coords = c("x", "y"), model.fun, model.args = list(), 
                       pred.fun = NULL, pred.args = list(), smp.fun = partition.loo, smp.args = list(), 
                       train.fun = NULL, train.param = NULL, test.fun = NULL, test.param = NULL, err.fun = err.default, 
                       error.fold = TRUE, error.rep = TRUE, err.train = TRUE, imp.variables = NULL, 
                       imp.permutations = 1000, importance = !is.null(imp.variables), distance = FALSE, 
                       par.args = list(par.mode = "foreach", par.units = NULL), 
                       do.gc = 1, do.try = FALSE, progress = 1, out.progress = "", # notify = FALSE, 
                       benchmark = FALSE, ...) {
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
    if (!error.fold) {
      warning(paste0("'importance = TRUE' currently only supported with 'error.fold = TRUE'.\n", 
                     "Using 'importance = FALSE'"))
      importance <- FALSE
    }
    
    stopifnot(is.numeric(imp.permutations))
    
    if (!is.null(imp.variables)) {
      stopifnot(is.character(imp.variables))
    }
  }
  stopifnot(is.character(coords))
  stopifnot(length(coords) == 2)
  
  if (importance & !error.fold) {
    stop("variable importance assessment currently only supported 
         at the unpooled level")
  }
  
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
  
  # Deprecated argumentsSun May 21 21:31:51 2017 ------------------------------
  dots.args <- list(...)
  if (length(dots.args) > 0) {
    if (any(names(dots.args) == "predfun")) {
      stop("sorry: argument names have changed; 'predfun' is now 'pred.fun'")
    }
    if (any(names(dots.args) == "silent")) {
      stop("sorry: argument names have changed; 'silent' is now 'progress'")
    }
    if (any(names(dots.args) == "err.pooled")) {
      stop("sorry: argument names have changed; 'err.pooled' is now 'error.rep'")
    }
    if (any(names(dots.args) == "err.unpooled")) {
      stop("sorry: argument names have changed; 'err.unpooled' is now 'error.fold'")
    }
    # > v1.1
    if (class(par.args$par.mode) == "numeric") {
      stop("par.mode has to be specified using an explicit parallel mode name")
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
  
  if (error.fold) {
    res <- lapply(resamp, unclass)
    class(res) <- "sperroresterror"
  } else {
    res <- NULL
  }
  
  pooled.err <- NULL
  
  # required to be able to assign levels to predictions if appropriate:
  is.factor.prediction <- NULL
  
  ### Permutation-based variable importance assessment (optional):
  impo <- NULL
  if (importance) {
    # Importance of which variables:
    if (is.null(imp.variables)) {
      imp.variables <- strsplit(as.character(formula)[3], " + ", fixed = TRUE)[[1]]
    }
    # Dummy data structure that will later be populated with the results:
    impo <- resamp
    # Create a template that will contain results of variable importance assessment:
    rep(NA, length(imp.variables)) %>% 
      as.list() -> imp.one.rep
      
    names(imp.one.rep) <- imp.variables
    
    rep(NA, imp.permutations) %>% 
      as.list() -> tmp

    names(tmp) <- as.character(1:imp.permutations)
    for (vnm in imp.variables) {
      imp.one.rep[[vnm]] <- tmp
    }
    rm(tmp)
  }
  
  
  ### par.mode = "apply" (pbapply) -------
  
  
  if (par.args$par.mode == "apply" | par.args$par.mode == "future" | par.args$par.mode == "apply-mclapply") {
    
    if (par.args$par.units > availableCores()) {
      par.args$par.units <- availableCores()
    }
    
    
    # parallelization here (par.mode = 1 & par.mode = 2) For each repetition:
    if (.Platform$OS.type == "windows") {
      par.cl <- makeCluster(par.args$par.units, type = "PSOCK")
      clusterSetRNGStream(par.cl, 1234567)  #set up RNG stream to obtain 
      # reproducible results
      force(pred.fun)  #force evaluation of pred.fun, so it is serialized and 
      # provided to all cluster workers
      clusterExport(par.cl, "par.args", envir = environment())
      # clusterEvalQ(par.cl, {
      #   map(X = par.args$par.libs, function(n)
      #   {
      #     do.call("library", list(n))
      #   })
      #   NULL
      # })
    } else {
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
    if (progress == FALSE) {
      pboptions(type = "none")
    } else {
      pboptions(style = 1, type = "timer")
    }
    
    #environment(runreps) <- environment()
    
    # runreps call Sun Apr  9 13:28:31 2017 ------------------------------
    
    if (par.args$par.mode == "apply" | par.args$par.mode == "apply-mclapply") {
      if (.Platform$OS.type == "Windows") {
        
        message(sprintf("Using 'parApply' parallel mode with %s cores.", par.args$par.units))
        myRes <- try(pblapply(cl = par.cl, resamp, function(X) runreps(currentSample = X, data = data, par.mode = par.args$par.mode,
                                                                       formula = formula, do.gc = do.gc, imp.one.rep = imp.one.rep, pred.fun = pred.fun,
                                                                       model.args = model.args, do.try = do.try, model.fun = model.fun,
                                                                       error.fold = error.fold, error.rep = error.rep, imp.permutations = imp.permutations,
                                                                       imp.variables = imp.variables, is.factor.prediction = is.factor.prediction,
                                                                       err.train = err.train, importance = importance, currentRes = currentRes,
                                                                       pred.args = pred.args, response = response, par.cl = par.cl,
                                                                       coords = coords, progress = progress, pooled.obs.train = pooled.obs.train,
                                                                       pooled.obs.test = pooled.obs.test, err.fun = err.fun)))
      } else {
        # not provided as an option to the user -> pbmclapply is faster
        # only used for performance evaluation
        if (par.args$par.mode == "apply-mclapply") { 
          message(sprintf("Using 'mclapply' parallel mode with %s cores.", par.args$par.units))
          myRes <- try(mclapply(mc.cores = par.cl, resamp, function(X) runreps(currentSample = X, data = data, par.mode = par.args$par.mode,
                                                                               formula = formula, do.gc = do.gc, imp.one.rep = imp.one.rep, pred.fun = pred.fun,
                                                                               model.args = model.args, do.try = do.try, model.fun = model.fun,
                                                                               error.fold = error.fold, error.rep = error.rep, imp.permutations = imp.permutations,
                                                                               imp.variables = imp.variables, is.factor.prediction = is.factor.prediction,
                                                                               err.train = err.train, importance = importance, currentRes = currentRes,
                                                                               pred.args = pred.args, response = response, par.cl = par.cl,
                                                                               coords = coords, progress = progress, pooled.obs.train = pooled.obs.train,
                                                                               pooled.obs.test = pooled.obs.test, err.fun = err.fun)))
          if (myRes == "NULL") {
            stop(paste0("No output was received from sperrorest.\n", 
                        "If you are on macOS either run R in 'Vanilla' mode or use another parallel mode."))
          }
        } else {
          message(sprintf("Using 'pbmclapply' parallel mode with %s cores.", par.args$par.units))
          myRes <- try(pbmclapply(mc.cores = par.cl, resamp, function(X) runreps(currentSample = X, data = data, par.mode = par.args$par.mode,
                                                                                 formula = formula, do.gc = do.gc, imp.one.rep = imp.one.rep, pred.fun = pred.fun,
                                                                                 model.args = model.args, do.try = do.try, model.fun = model.fun,
                                                                                 error.fold = error.fold, error.rep = error.rep, imp.permutations = imp.permutations,
                                                                                 imp.variables = imp.variables, is.factor.prediction = is.factor.prediction,
                                                                                 err.train = err.train, importance = importance, currentRes = currentRes,
                                                                                 pred.args = pred.args, response = response, par.cl = par.cl,
                                                                                 coords = coords, progress = progress, pooled.obs.train = pooled.obs.train,
                                                                                 pooled.obs.test = pooled.obs.test, err.fun = err.fun)))
          # check if run was sufficient
          if (length(myRes) > 1 && myRes == "NULL") {
            stop(paste0("No output was received from sperrorest.\n", 
                        "If you are on macOS either run R in 'Vanilla' mode or use another parallel mode."))
          }
        }
      }
    }
    
    # par.mode = "future" Sun May 21 12:04:55 2017 ------------------------------
    
    if (par.args$par.mode == "future") {
      if (!is.null(par.args$par.option)) {
        plan(par.args$par.option, workers = par.args$par.units)
      } else {
        par.args$par.option <- "multiprocess"
        plan(par.args$par.option, workers = par.args$par.units)
      }
      
      message(sprintf("Using parallel framework 'future' with 'future_lapply'  and '%s' option.", par.args$par.option))
      myRes <- try(future_lapply(resamp, function(X) runreps(currentSample = X, data = data, par.mode = par.args$par.mode,
                                                             formula = formula, do.gc = do.gc, imp.one.rep = imp.one.rep, pred.fun = pred.fun,
                                                             model.args = model.args, do.try = do.try, model.fun = model.fun,
                                                             error.fold = error.fold, error.rep = error.rep, imp.permutations = imp.permutations,
                                                             imp.variables = imp.variables, is.factor.prediction = is.factor.prediction,
                                                             err.train = err.train, importance = importance, currentRes = currentRes,
                                                             pred.args = pred.args, response = response, par.cl = par.cl,
                                                             coords = coords, progress = progress, pooled.obs.train = pooled.obs.train,
                                                             pooled.obs.test = pooled.obs.test, err.fun = err.fun)))
    }
  }
  
  ### par.mode = "foreach" -------
  
  if (par.args$par.mode == "foreach" | par.args$par.mode == "sequential") {
    
    # combine function for multiple object outputs in foreach call
    comb <- function(...)
    {
      mapply("rbind", ..., SIMPLIFY = FALSE)
    }
    
    # suppress any progress output of workes if progress = FALSE
    if (progress == FALSE) {
      out.progress <- "/dev/null"
      if (Sys.info()["sysname"] == "Windows") {
        out.progress <- "nul:"
      }
    }
    # special settings for Windows
    if (out.progress == "" & Sys.info()["sysname"] == "Windows") {
      out.progress <- paste0(getwd(), "/sperrorest.progress.txt")
    }

    registerDoFuture()

    # check for sequential/parallel execution and (if parallel) get number of cores
    if (is.null(par.args$par.units) && !par.args$par.mode == "sequential") {
      plan(multiprocess)
      cores <- availableCores()
      message(sprintf("Using 'foreach' parallel mode with %s cores.", cores))
    } 
    if (!is.null(par.args$par.units) && !par.args$par.mode == "sequential") {
      plan(multiprocess, workers = par.args$par.units)
      message(sprintf("Using 'foreach' parallel mode with %s cores.", par.args$par.units))
    }
    if (par.args$par.mode == "sequential") {
      plan(sequential)
      message(sprintf("Using 'foreach' sequential mode."))
    }
    
    # runreps call Fri May 19 14:35:58 2017 ------------------------------
    
    myRes <- foreach(i = 1:length(resamp), .packages = (.packages()), .errorhandling = "remove", 
                     .combine = "comb", .multicombine = TRUE, .verbose = FALSE) %dopar% {
                       
                       if (err.train) {
                         pooled.obs.train <- pooled.pred.train <- c()
                         pooled.obs.test <- pooled.pred.test <- c()
                       }
                       
                       currentRes <- NULL
                       currentImpo <- resamp[[i]]
                       currentPooled.err <- NULL
                       
                       
                       if (error.fold) {
                         currentRes <- map(resamp[[i]], unclass)
                         class(currentRes) <- "sperroresterror"
                       } else {
                         currentRes <- NULL
                       }
                       environment(runfolds) <- environment()
                       
                       if (progress == 2) {
                         cat(date(), "Repetition", names(resamp[[i]])[i], "\n")
                       }
                       
                       try(map(seq_along(resamp[[i]]), 
                               function(rep) runfolds(j = rep, data = data, currentSample = resamp[[i]],
                                                      formula = formula, par.mode = par.args$par.mode, i = i, 
                                                      imp.one.rep = imp.one.rep, pred.fun = pred.fun, 
                                                      model.args = model.args, do.try = do.try, model.fun = model.fun,
                                                      error.fold = error.fold, error.rep = error.rep, imp.permutations = imp.permutations,
                                                      imp.variables = imp.variables, is.factor.prediction = is.factor.prediction,
                                                      err.train = err.train, importance = importance, currentRes = currentRes,
                                                      pred.args = pred.args, response = response, par.cl = par.cl,
                                                      coords = coords, progress = progress, pooled.obs.train = pooled.obs.train,
                                                      pooled.obs.test = pooled.obs.test, err.fun = err.fun))) -> runfolds_list
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
                       
                       # Put the results from the pooled estimation into the pooled.err data structure:
                       if (error.rep) {
                         if (is.factor(data[, response])) {
                           lev <- levels(data[, response])
                           if (err.train) {
                             lev[pooled_only$pooled.obs.train] %>% 
                               factor(levels = lev) -> pooled_only$pooled.obs.train
                             # pooled_only$pooled.obs.train <- factor(lev[pooled_only$pooled.obs.train], levels = lev)
                           }
                           lev[pooled_only$pooled.obs.test] %>% 
                             factor(levels = lev) -> pooled_only$pooled.obs.test
                           # pooled_only$pooled.obs.test <- factor(lev[pooled_only$pooled.obs.test], levels = lev)
                           if (is.factor.prediction) {
                             if (err.train) {
                               lev[pooled_only$pooled.pred.train] %>% 
                                 factor(levels = lev) -> pooled_only$pooled.pred.train
                               # pooled_only$pooled.pred.train <- factor(lev[pooled_only$pooled.pred.train], levels = lev)
                             }
                             lev[pooled_only$pooled.obs.test] %>% 
                               factor(levels = lev) -> pooled_only$pooled.obs.test
                             # pooled_only$pooled.pred.test <- factor(lev[pooled_only$pooled.pred.test], levels = lev)
                           }
                         } 
                         pooled.err.train <- NULL
                         if (err.train) {
                           pooled.err.train <- err.fun(pooled_only$pooled.obs.train, pooled_only$pooled.pred.train)
                         }
                         
                         list(train = pooled.err.train, test = err.fun(pooled_only$pooled.obs.test,
                                                                       pooled_only$pooled.pred.test)) %>% 
                                unlist() %>% 
                                t() -> currentPooled.err
                         
                         # currentPooled.err <- t(unlist(list(train = pooled.err.train, test = err.fun(pooled_only$pooled.obs.test,
                         #                                                                           pooled_only$pooled.pred.test))))
                         if (do.gc >= 2) {
                           gc()
                         }
                       }
                       
                       if ((do.gc >= 1) & (do.gc < 2)) {
                         gc()
                       }
                       
                       # set currentImpo to NULL to prevent false importance output (resamp object)
                       # if importance == FALSE
                       if (importance == FALSE) {
                         impo_only <- NULL
                       }
                       result <- list(error = runfolds_merged$currentRes, pooled.error = currentPooled.err, importance = impo_only)
                       return(list(result))
                     }
  }
  
  ### format parallel outputs ----
  
  if (par.args$par.mode == "foreach" | par.args$par.mode == "sequential") {
    # split combined lists from foreach output into sublists referring to repetitions 
    myRes <- split(myRes[[1]], 1:length(resamp))
  }
  
  # assign names to sublists - otherwise `transfer_parallel_output` does not work
  for (i in 1:length(myRes)) {
    names(myRes[[i]]) <- c("error", "pooled.error", "importance")
  }
  
  # transfer results of lapply() to respective data objects
  myRes_mod <- transfer_parallel_output(myRes, res, impo, pooled.err)
  
  # convert matrix(?) to data.frame:
  if (error.rep) {
    pooled.err <- as.data.frame(myRes_mod$pooled.err)
    rownames(pooled.err) <- NULL
    class(pooled.err) <- "sperrorestreperror"
  }
  
  if (importance) {
    impo <- myRes_mod$impo
    class(impo) <- "sperrorestimportance"
  }
  
  if (benchmark) {
    end.time <- Sys.time()
    my.bench <- list(system.info = Sys.info(), t.start = start.time, t.end = end.time, 
                     cpu.cores = detectCores(), par.mode = par.args$par.mode, 
                     par.units = par.args$par.units, runtime.performance = end.time - start.time)
    class(my.bench) <- "sperrorestbenchmarks"
  } else {
    my.bench <- NULL
  }
  
  package.version <- packageVersion("sperrorest")
  class(package.version) <- "sperrorestpackageversion"
  
  RES <- list(error.rep = pooled.err, error.fold = myRes_mod$res, represampling = resamp, 
              importance = impo, benchmarks = my.bench, package.version = package.version)
  class(RES) <- "sperrorest"
  
  return(RES)
  }


