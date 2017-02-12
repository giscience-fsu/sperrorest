
#' Summarize error statistics obtained by `sperrorest`
#'
#' `summary.sperroresterror` calculates mean, standard deviation, 
#' median etc. of the calculated error measures at the specified level 
#' (overall, repetition, or fold).
#' `summary.sperrorestreperror` does the same with the pooled error, 
#' at the overall or repetition level.
#' 
#' @import rpart
#' @importFrom stats IQR kmeans mad median predict
#' rnorm runif sd terms weighted.mean 
#' @importFrom graphics par plot points
#' @importFrom ROCR performance prediction
#' @name summary.sperroresterror
#' @method summary sperroresterror
#' 
#' @param object `sperroresterror` resp. `sperrorestcombinederror` 
#' error object calculated by [sperrorest()]
#' @param level Level at which errors are summarized: 
#' 0: overall; 1: repetition; 2: fold
#' @param pooled If `TRUE` (default), mean and standard deviation etc are 
#' calculated between fold-level error estimates. If `FALSE`, 
#' apply first a [weighted.mean()] among folds before calculating 
#' mean, standard deviation etc among repetitions. See also Details.
#' @param na.rm Remove `NA` values? See [mean()] etc.
#' @param ... additional arguments (currently ignored)
#' 
#' @return Depending on the level of aggregation, a `list` or 
#' `data.frame` with mean, and at level 0 also standard deviation, 
#' median and IQR of the error measures.
#' 
#' @details Let's use an example to explain the `error.rep` argument. 
#' E.g., assume we are using 100-repeated 10-fold cross-validation. 
#' If `error.rep = TRUE` (default), the mean and standard deviation calculated 
#' when summarizing at `level = 0`
#' are calculated across the error estimates obtained for 
#' each of the `100*10 = 1000` folds.
#' If `error.rep = FALSE`, mean and standard deviation are calculated across 
#' the `100` repetitions, using the weighted average of the fold-level 
#' errors to calculate an error value for the entire sample. 
#' This will essentially not affect the mean value but of course the 
#' standard deviation of the error. `error.rep = FALSE` is not recommended, 
#' it is mainly for testing purposes; when the test sets are small 
#' (as in leave-one-out cross-validation, in the extreme case), 
#' consider running [sperrorest()] with `error.rep = TRUE` and 
#' examine only the `error.rep` component of its result.
#' 
#' @seealso [sperrorest::parsperrorest()]
#' 
#' @export
summary.sperroresterror <- function(object, level = 0, pooled = TRUE, na.rm = TRUE, 
  ...)
  {
  err <- unclass(object)
  if (pooled)
  {
    if (level <= 2) 
      err <- lapply(err, function(x) t(sapply(x, function(y) data.frame(train = y$train, 
        test = y$test, distance = ifelse(any(names(y) == "distance"), y$distance, 
          -1)))))
    if (level <= 1)
    {
      errdf <- err[[1]]
      if (length(err) > 1)
      {
        for (i in 2:length(err))
        {
          errdf <- rbind(errdf, err[[i]])
        }
      }
      rownames(errdf) <- NULL
      err <- as.data.frame(errdf)
    }
    if (level <= 0)
    {
      err <- data.frame(mean = apply(err, 2, function(y) mean(unlist(y), na.rm = na.rm)), 
        sd = apply(err, 2, function(y) sd(unlist(y), na.rm = na.rm)), median = apply(err, 
          2, function(y) median(unlist(y), na.rm = na.rm)), IQR = apply(err, 
          2, function(y) IQR(unlist(y), na.rm = na.rm)))
    }
  } else
  {
    if (level <= 2) 
      err <- lapply(err, function(x) t(sapply(x, function(y) data.frame(train = y$train, 
        test = y$test, distance = ifelse(any(names(y) == "distance"), y$distance, 
          -1)))))
    if (level <= 1)
    {
      ### w = summary.partition(resampling) ?????
      err <- lapply(err, function(x) apply(x, 2, function(y) weighted.mean(unlist(y), 
        na.rm = na.rm)))
      nms <- names(err)
      err <- as.data.frame(t(as.data.frame(err)))
      rownames(err) <- nms
    }
    if (level <= 0)
    {
      err <- data.frame(mean = sapply(err, mean), sd = sapply(err, sd), median = sapply(err, 
        median), IQR = sapply(err, IQR))
    }
  }
  return(err)
}

#' @rdname summary.sperrorest
#' @inheritParams summary.sperroresterror
#' @name summary.sperrorestreperror
#' @method summary sperrorestreperror
#' @export
summary.sperrorestreperror <- function(object, level = 0, na.rm = TRUE, ...)
{
  class(object) <- NULL
  object <- as.data.frame(object)
  if (level <= 0)
  {
    object <- data.frame(mean = sapply(object, mean), sd = sapply(object, sd), 
      median = sapply(object, median), IQR = sapply(object, IQR))
  }
  return(object)
}

#' Summarize variable importance statistics obtained by `sperrorest`
#'
#' `summary.sperrorestimportance` calculated mean, standard deviation, 
#' median etc. of the calculated error measures at the specified level 
#' (overall, repetition, or fold).
#' @name summary.sperrorestimportance
#' @method summary sperrorestimportance
#' 
#' @param object [sperrorestimportance()] object calculated by 
#' [sperrorest()] called with argument `importance=TRUE`
#' @inheritParams summary.sperroresterror
#' @param which optional character vector specifying selected variables for 
#' which the importances should be summarized (to do: check implementation)
#' 
#' @return a list or data.frame, depending on the `level` of aggregation
#' 
#' @export
summary.sperrorestimportance <- function(object, level = 0, na.rm = TRUE, which = NULL, 
  ...)
  {
  arrdim <- c(length(object), length(object[[1]]), dim(object[[1]][[1]]))
  arrdimnames <- list(names(object), names(object[[1]]), rownames(object[[1]][[1]]), 
    colnames(object[[1]][[1]]))
  arr <- array(NA, dim = arrdim, dimnames = arrdimnames)
  for (i in 1:length(object)) for (j in 1:length(object[[i]])) arr[i, j, , ] <- as.matrix(object[[i]][[j]])
  if (level <= 1) 
    arr <- apply(arr, c(1, 3, 4), mean, na.rm = na.rm)
  if (level <= 0)
  {
    if (is.null(which))
    {
      arr <- data.frame(mean = apply(arr, c(2, 3), mean, na.rm = na.rm), sd = apply(arr, 
        c(2, 3), sd, na.rm = na.rm), median = apply(arr, c(2, 3), median, 
        na.rm = na.rm), IQR = apply(arr, c(2, 3), IQR, na.rm = na.rm))
    } else
    { # when does this happen?
      arr <- arr[, , which] # nocov
      arr <- data.frame(mean = apply(arr, 2, mean, na.rm = na.rm), sd = apply(arr, # nocov
        2, sd, na.rm = na.rm), median = apply(arr, 2, median, na.rm = na.rm), # nocov
        IQR = apply(arr, 2, IQR, na.rm = na.rm)) # nocov
    }
  }
  return(arr)
}

#' Summarize benchmark information obtained by `sperrorest`
#' 
#' `summary.sperrorestbenchmarks` shows information on runtime performance, 
#' used cores and system information
#' @name summary.sperrorestbenchmarks
#' @method summary sperrorestbenchmarks
#' @inheritParams summary.sperroresterror
#' 
#' @param object `sperrorestbenchmarks` object returned class by 
#' [sperrorest()] 
#' @return List of length seven
#' 
#' @export
summary.sperrorestbenchmarks <- function(object, ...) {
  class(object) <- NULL
  object <- unlist(object)
  object <- as.matrix(object)
  colnames(object) <- "benchmarks"
  return(object)
}

#' Summarize package version information obtained by `sperrorest`
#' 
#' `summary.sperrorestpackageversion` returns the package version of sperrorest
#' @name summary.sperrorestpackageversion
#' @method summary sperrorestpackageversion
#' @inheritParams summary.sperroresterror
#' 
#' @param object `sperrorestpackageversion` object calculated by 
#' [sperrorest()] 
#' @return character vector of length one
#' 
#' @export
summary.sperrorestpackageversion <- function(object, ...) {
  paste(object[[1]], collapse = ".")
}

#' Draw stratified random sample
#'
#' `resample.strat.uniform` draws a stratified random sample 
#' (with or without replacement) from the samples in `data`. 
#' Stratification is over the levels of `data[,param$response]`. 
#' The same number of samples is drawn within each level.
#' 
#' @param data a `data.frame`, rows represent samples
#' @param param a list with the following components: `strat` is either 
#' the name of a factor variable in `data` that defines the stratification
#'  levels, or a vector of type factor and length `nrow(data)`; 
#'  `n` is a numeric value specifying the size of the subsample; 
#'  `replace` determines if sampling is with or without replacement
#'  
#' @return a `data.frame` containing a subset of the rows of `data`.
#' 
#' @details If `param$replace=FALSE`, a subsample of size 
#' `min(param$n,nrow(data))` will be drawn from `data`. 
#' If `param$replace=TRUE`, the size of the subsample is `param$n`.
#' 
#' @seealso [resample.uniform()], [sample()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' d = resample.strat.uniform(ecuador, 
#'                            param = list(strat = 'slides', nstrat = 100))
#' nrow(d) # == 200
#' sum(d$slides == 'TRUE') # == 100
#' 
#' @export
resample.strat.uniform <- function(data, param = list(strat = "class", nstrat = Inf, 
  replace = FALSE))
  {
  # Old version:
  if (!is.null(param$response))
  {
    warning("'param$response' argument in 'resample.strat.uniform' renamed 
        to 'strat';\n modify your code accordingly")
    if (is.null(param$strat)) 
      param$strat <- param$response
  }
  
  # Use defaults if not specified:
  if (is.null(param$strat)) 
    param$strat <- "class"
  if (is.null(param$nstrat)) 
    param$nstrat <- Inf
  if (is.null(param$replace)) 
    param$replace <- FALSE
  
  stopifnot((length(param$strat) == 1) | (length(param$strat) == nrow(data)))
  if (length(param$strat == 1))
  {
    strat <- data[, param$strat]
  } else strat <- param$strat
  if (!is.factor(strat)) 
    stop("'strat' must either be a vector of factor type, or the name of 
       a factor variable in 'data'")
  # Each factor level must have at least one sample, otherwise sampling within this
  # level is impossible:
  minstrat <- min(tapply(strat, strat, length))
  stopifnot(minstrat >= 1)
  # might want to change this to a warning.???
  
  if (!param$replace) 
    param$nstrat <- min(param$nstrat, minstrat)
  
  # Uniform sampling within each stratum:
  sel <- c()
  for (lev in levels(strat))
  {
    wh <- sample(which(strat == lev), size = param$nstrat, replace = param$replace)
    sel <- c(sel, wh)
  }
  return(data[sel, ])
}
# To do: allow nstrat to be a named vector


#' Draw uniform random (sub)sample
#'
#' `resample.uniform` draws a random (sub)sample 
#' (with or without replacement) from the samples in `data`.
#' 
#' @param data a `data.frame`, rows represent samples
#' @param param a list with the following components: `n` is a numeric 
#' value specifying the size of the subsample; `replace` determines if 
#' sampling is with or without replacement
#' 
#' @return a `data.frame` containing a subset of the rows of `data`.
#' 
#' @details If `param$replace=FALSE`, a subsample of size 
#' `min(param$n,nrow(data))` will be drawn from `data`. 
#' If `param$replace=TRUE`, the size of the subsample is `param$n`.
#' 
#' @seealso [resample.strat.uniform()], [sample()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' d = resample.uniform(ecuador, param = list(strat = 'slides', n = 200))
#' nrow(d) # == 200
#' sum(d$slides == 'TRUE')
#' 
#' @export
resample.uniform <- function(data, param = list(n = Inf, replace = FALSE))
{
  # Apply defaults if missing from parameter list:
  if (is.null(param$n)) 
    param$n <- Inf
  if (is.null(param$replace)) 
    param$replace <- FALSE
  
  if (!param$replace) 
    param$n <- min(param$n, nrow(data))
  
  # Uniform sampling with or without replacement:
  sel <- sample(nrow(data), size = param$n, replace = param$replace)
  
  return(data[sel, ])
}



#' Draw uniform random (sub)sample at the group level
#'
#' `resample.factor` draws a random (sub)sample 
#' (with or without replacement) of the groups or clusters identified by 
#' the `fac` argument.
#' 
#' @param data a `data.frame`, rows represent samples
#' @param param a list with the following components: `fac` is a factor 
#' variable of length `nrow(data)` or the name of a factor variable 
#' in `data`; `n` is a numeric value specifying the size of the 
#' subsample (in terms of groups, not observations); `replace` determines 
#' if resampling of groups is to be done with or without replacement.
#' 
#' @return a `data.frame` containing a subset of the rows of `data`.
#' 
#' @details If `param$replace=FALSE`, a subsample of 
#' `min(param$n,nlevel(data[,fac]))` groups will be drawn from `data`. 
#' If `param$replace=TRUE`, the number of groups to be drawn is `param$n`.
#' 
#' @seealso [resample.strat.uniform()], [sample()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' d = resample.uniform(ecuador, param = list(strat = 'slides', n = 200))
#' nrow(d) # == 200
#' sum(d$slides == 'TRUE')
#' 
#' @export
resample.factor <- function(data, param = list(fac = "class", n = Inf, replace = FALSE))
{
  if (is.null(param$fac)) 
    param$fac <- "class"
  if (is.null(param$replace)) 
    param$replace <- FALSE
  stopifnot((length(param$fac) == 1) || (length(param$fac) == nrow(data)))
  if (length(param$fac == 1))
  {
    fac <- data[, param$fac]
  } else fac <- param$fac
  if (!is.factor(fac)) 
    stop("'fac' must either be a vector of factor type, or the name of a 
     factor variable in 'data'")
  fac <- factor(fac)
  if (is.null(param$n) || is.infinite(param$n)) 
    param$n <- nlevels(fac)
  if (!param$replace) 
    param$n <- min(param$n, nrow(data))
  sel <- sample(levels(fac), size = param$n, replace = param$replace)
  sel <- fac %in% sel
  return(data[sel, ])
}

#' Perform spatial error estimation and variable importance assessment
#'
#' `sperrorest` is a flexible interface for multiple types of spatial and 
#' non-spatial cross-validation and bootstrap error estimation and 
#' permutation-based assessment of spatial variable importance.
#' 
#' @importFrom utils packageVersion
#' @import rpart
#' 
#' @inheritParams partition.cv
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
#' 
#' @param model.args Arguments to be passed to `model.fun` 
#' (in addition to the `formula` and `data` argument, 
#' which are provided by `sperrorest`)
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
#' @param error.fold logical (default: `TRUE` if `importance` is 
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
#' @param progress if `progress = 1`, repetition and fold progress is 
#' shown in console (in Windows Rgui, disable 'Buffered output' in 'Misc' menu). 
#' If `progress = 2`, only repetition information is shown. 
#' Set to `FALSE` for no progress information. 
#' 
#' @param notify (optional) show a notification badge after `sperrorest()` has finished.
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
#' @export
#' 
#' @aliases sperroresterror sperrorestimportance
#' 
#' @seealso [parsperrorest()] 
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' 
#' # Example of a classification tree fitted to this data:
#' library(rpart)
#' ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
#' fit <- rpart(fo, data = ecuador, control = ctrl)
#' par(xpd = TRUE)
#' plot(fit, compress = TRUE, main = 'Stoyans landslide data set')
#' text(fit, use.n = TRUE)
#'
#' # Non-spatial 5-repeated 10-fold cross-validation:
#' mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
#' nspres <- sperrorest(data = ecuador, formula = fo,
#'                      model.fun = rpart, model.args = list(control = ctrl),
#'                      pred.fun = mypred.rpart,
#'                      smp.fun = partition.cv, 
#'                      smp.args = list(repetition = 1:5, nfold = 10))
#' summary(nspres$error.rep)                    
#' summary(nspres$error.fold)
#' summary(nspres$represampling)
#' # plot(nspres$represampling, ecuador)
#'
#' # Spatial 5-repeated 10-fold spatial cross-validation:
#' spres <- sperrorest(data = ecuador, formula = fo,
#'                     model.fun = rpart, model.args = list(control = ctrl),
#'                     pred.fun = mypred.rpart,
#'                     smp.fun = partition.kmeans, 
#'                     smp.args = list(repetition = 1:5, nfold = 10))
#' summary(spres$error.rep)
#' summary(spres$represampling)
#' # plot(spres$represampling, ecuador)
#' 
#' smry <- data.frame(
#'      nonspat.training = unlist(summary(nspres$error.rep, level = 1)$train.auroc),
#'      nonspat.test     = unlist(summary(nspres$error.rep, level = 1)$test.auroc),
#'      spatial.training = unlist(summary(spres$error.rep,  level = 1)$train.auroc),
#'      spatial.test     = unlist(summary(spres$error.rep,  level = 1)$test.auroc))
#' boxplot(smry, col = c('red','red','red','green'), 
#'         main = 'Training vs. test, nonspatial vs. spatial',
#'         ylab = 'Area under the ROC curve')
sperrorest <- function(formula, data, coords = c("x", "y"), model.fun, model.args = list(), 
  pred.fun = NULL, pred.args = list(), smp.fun = partition.loo, smp.args = list(), 
  train.fun = NULL, train.param = NULL, test.fun = NULL, test.param = NULL, err.fun = err.default, 
  error.fold = TRUE, error.rep = TRUE, err.train = TRUE, imp.variables = NULL, 
  imp.permutations = 1000, importance = !is.null(imp.variables), distance = FALSE, 
  do.gc = 1, do.try = FALSE, progress = 1, benchmark = FALSE, notify = FALSE, ...)
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
      warning("'importance = TRUE' currently only supported with 
          'error.fold = TRUE'.\nUsing 'importance = FALSE'")
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
  
  # For each repetition:
  for (i in 1:length(resamp))
  {
    if (progress == 1 | progress == 2)
    {
      cat(date(), "Repetition", names(resamp)[i], "\n")
    }
    
    # Collect pooled results in these data structures:
    if (err.train)
    {
      pooled.obs.train <- pooled.pred.train <- c()
      pooled.obs.test <- pooled.pred.test <- c()
    }
    
    # Parallelize this???  For each fold:
    for (j in 1:length(resamp[[i]]))
    {
      
      if (progress == 1)
      {
        cat(date(), "- Fold", j, "\n")
      }
      
      # 'silent' setting of try() calls
      if (progress == 1 | progress == 2)
      {
        silent <- FALSE
      } else (silent <- TRUE)
      
      # Create training sample:
      nd <- data[resamp[[i]][[j]]$train, ]
      if (!is.null(train.fun))
      {
        nd <- train.fun(data = nd, param = train.param) # nocov
      }
      # Train model on training sample:
      margs <- c(list(formula = formula, data = nd), model.args)
      
      if (do.try)
      {
        fit <- try(do.call(model.fun, args = margs), silent = silent)
        
        # Error handling:
        if (class(fit) == "try-error") # when does this happen?
        {
          fit <- NULL # nocov
          if (error.fold) # nocov
          { # nocov
          if (err.train) # nocov
            res[[i]][[j]]$train <- NULL # nocov
          res[[i]][[j]]$test <- NULL # nocov
          if (importance) # nocov
            impo[[i]][[j]] <- c()  # nocov # ???
          } # nocov
          if (do.gc >= 2) # nocov
          gc() # nocov
          next  # nocov # skip this fold
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
        {
          if (do.try)
          {
          err.try <- try(err.fun(nd[, response], pred.train), silent = silent)
          if (class(err.try) == "try-error") 
            err.try <- NULL  # ???
          res[[i]][[j]]$train <- err.try
          } else
          {
          res[[i]][[j]]$train <- err.fun(nd[, response], pred.train)
          }
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
      
      # Create test sample:
      nd <- data[resamp[[i]][[j]]$test, ]
      if (!is.null(test.fun)) 
        nd <- test.fun(data = nd, param = test.param)
      # Create a 'backup' copy for variable importance assessment:
      if (importance) 
        nd.bak <- nd
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
          err.try <- try(err.fun(nd[, response], pred.test), silent = silent)
          if (class(err.try) == "try-error")
          {
          err.try <- NULL # nocov (previous: '???')
          }
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
      ### Permutation-based variable importance assessment:
      if (importance & error.fold)
      {
        
        if (is.null(res[[i]][[j]]$test)) # does this ever happen?
        {
          impo[[i]][[j]] <- c() #nocov
          if (progress == 1 | progress == 2) #nocov
          { # nocov
          cat(date(), "-- skipping variable importance\n") #nocov
          } #nocov
        } else
        {
          
          if (progress == 1 | progress == 2)
          {
          cat(date(), "-- Variable importance\n")
          }
          imp.temp <- imp.one.rep
          
          # Parallelize this: ???
          for (cnt in 1:imp.permutations)
          {
          # Some output on screen:
          if (progress == 1 | progress == 2 & (cnt > 1))
          {
            if (log10(cnt) == floor(log10(cnt)))
            {
            cat(date(), "   ", cnt, "\n")
            }
          }
          
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
            permut.err <- try(err.fun(nd[, response], pred.test), silent = silent)
            if (class(permut.err) == "try-error")
            {
              imp.temp[[vnm]][[cnt]] <- c() # nocov (previous: '???')
            } else
            {
              imp.temp[[vnm]][[cnt]] <- as.list(unlist(res[[i]][[j]]$test) - 
              unlist(permut.err))
              # (apply '-' to corresponding list elements; only works if all list elements are
              # scalars)
            }
            } else
            {
            permut.err <- err.fun(nd[, response], pred.test)
            imp.temp[[vnm]][[cnt]] <- as.list(unlist(res[[i]][[j]]$test) - 
              unlist(permut.err))
            # (apply '-' to corresponding list elements; only works if all list elements are
            # scalars)
            }
          }
          }
          # average the results obtained in each permutation:
          impo[[i]][[j]] <- as.data.frame(t(sapply(imp.temp, function(y) sapply(as.data.frame(t(sapply(y, 
          as.data.frame))), function(x) mean(unlist(x))))))
          rm(nd.bak, nd)  # better safe than sorry...
        }  # end of else if (!is.null(res[[i]][[j]]$test))
      }
      
    }
    
    # Put the results from the pooled estimation into the pooled.err data structure:
    if (error.rep) 
      {
        if (is.factor(data[, response]))
        {
          lev <- levels(data[, response])
          if (err.train) {
            pooled.obs.train <- factor(lev[pooled.obs.train], levels = lev)
            pooled.obs.test <- factor(lev[pooled.obs.test], levels = lev)
          }
          if (is.factor.prediction) {
            if (err.train) {
              pooled.pred.train <- factor(lev[pooled.pred.train], levels = lev)
              pooled.pred.test <- factor(lev[pooled.pred.test], levels = lev)
            }
          }
        }
        pooled.err.train <- NULL
        if (err.train)
        {
          pooled.err.train <- err.fun(pooled.obs.train, pooled.pred.train)
        }
        if (i == 1)
        {
          pooled.err <- t(unlist(list(train = pooled.err.train, test = err.fun(pooled.obs.test, 
          pooled.pred.test))))
        } else
        {
          pooled.err <- rbind(pooled.err, unlist(list(train = pooled.err.train, 
          test = err.fun(pooled.obs.test, pooled.pred.test))))
        }
        
        if (do.gc >= 2)
        {
          gc()
        }
      }  # end for each fold
    
    if ((do.gc >= 1) & (do.gc < 2))
    {
      gc()
    }
  }  # end for each repetition
  
  # convert matrix(?) to data.frame:
  if (error.rep)
  {
    pooled.err <- as.data.frame(pooled.err)
    rownames(pooled.err) <- NULL
    class(pooled.err) <- "sperrorestreperror"
  }
  
  if (progress == 1 | progress == 2)
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
      cpu.cores = detectCores(), par.mode = NA, par.units = NA, runtime.performance = end.time - 
        start.time)
    class(my.bench) <- "sperrorestbenchmarks"
  } else my.bench <- NULL
  
  if (notify == TRUE) {
    if (benchmark == TRUE) {
      msg <- paste0("Repetitions: ", length(smp.args$repetition), "; ", 
                    "Folds: ", smp.args$nfold, "; ", "Total time: ", round(my.bench$runtime.performance, 
                                                                           2))
    } else (msg <- paste0("Repetitions: ", length(smp.args$repetition), "; ", 
                          "Folds: ", smp.args$nfold))
  
    notify(title = "sperrorest() finished successfully!", msg <- msg)
  }
  
  package.version <- packageVersion("sperrorest")
  class(package.version) <- "sperrorestpackageversion"
  
  RES <- list(error.rep = pooled.err, error.fold = res, represampling = resamp, 
              importance = impo, benchmarks = my.bench, package.version = package.version)
  class(RES) <- "sperrorest"
  
  return(RES)
}


#' Summary and print methods for sperrorest results
#'
#' Summary methods provide varying level of detail while print methods 
#' provide full details.
#' @name summary.sperrorest
#' 
#' @method summary sperrorest
#' 
#' @param object a [sperrorest()] object
#' @param ... additional arguments for [summary.sperroresterror()] 
#' or [summary.sperrorestimportance()]
#' @param x Depending on method, a [sperrorest()], 
#' [sperroresterror()] or [sperrorestimportance()] object
#' 
#' @seealso [sperrorest()], [sperroresterror()], 
#' [sperrorestimportance()], 
#' [summary.sperroresterror()], 
#' [summary.sperrorestimportance()]
#' 
#' @export
summary.sperrorest <- function(object, ...)
{
  list(error.rep = summary(object$error.rep, ...), error.fold = summary(object$error.fold, 
    ...), represampling = summary(object$represampling, ...), importance = summary(object$importance, 
    ...), benchmark = summary(object$benchmark, ...), packageVersion = summary(object$package.version, ...))
}

#' @rdname summary.sperrorest
#' @name print.sperrorestimportance
#' @method print sperrorestimportance
#' @export
print.sperrorestimportance <- function(x, ...) print(unclass(summary(x, level = Inf, 
  ...)))

#' @rdname summary.sperrorest
#' @name print.sperroresterror
#' @method print sperroresterror
#' @export
print.sperroresterror <- function(x, ...) print(unclass(summary(x, level = Inf, ...)))

#' @rdname summary.sperrorest
#' @name print.sperrorestreperror
#' @method print sperrorestreperror
#' @export
print.sperrorestreperror <- function(x, ...) print(unclass(summary(x, level = Inf, 
  ...)))

#' @rdname summary.sperrorest
#' @name print.sperrorest
#' @method print sperrorest
#' @export
print.sperrorest <- function(x, ...) print(unclass(summary(x, level = Inf, ...)))

#' @rdname summary.sperrorest
#' @name print.sperrorestbenchmarks
#' @method print sperrorestbenchmarks
#' @export
print.sperrorestbenchmarks <- function(x, ...) print(summary(x), ...)

#' @rdname summary.sperrorest
#' @name print.sperrorestpackageversion
#' @method print sperrorestpackageversion
#' @export
print.sperrorestpackageversion <- function(x, ...) print(summary(x))



