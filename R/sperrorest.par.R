#' Perform spatial error estimation and variable importance assessment in parallel
#'
#' \code{sperrorest} is a flexible interface for multiple types of spatial and 
#' non-spatial cross-validation and bootstrap error estimation and 
#' permutation-based assessment of spatial variable importance.
#' 
#' @import foreach
#' @import doParallel
#' @import rpart
#' @importFrom utils packageVersion
#' @importFrom parallel detectCores 
#' @import ggplot2 
#' 
#' @inheritParams partition.cv
#' 
#' @param data a \code{data.frame} with predictor and response variables. 
#' Training and test samples will be drawn from this data set by \code{train.fun} 
#' and \code{test.fun}, respectively.
#' @param formula A formula specifying the variables used by the \code{model}. 
#' Only simple formulas without interactions or nonlinear terms should 
#' be used, e.g. \code{y~x1+x2+x3} but not \code{y~x1*x2+log(x3)}. 
#' Formulas involving interaction and nonlinear terms may possibly work 
#' for error estimation but not for variable importance assessment, 
#' but should be used with caution.
#' @param coords vector of length 2 defining the variables in \code{data} that 
#' contain the x and y coordinates of sample locations.
#' @param model.fun Function that fits a predictive model, such as \code{glm} 
#' or \code{rpart}. The function must accept at least two arguments, the first 
#' one being a formula and the second a data.frame with the learning sample.
#' @param model.args Arguments to be passed to \code{model.fun} 
#' (in addition to the \code{formula} and \code{data} argument, 
#' which are provided by \code{sperrorest})
#' @param pred.fun Prediction function for a fitted model object created 
#' by \code{model}. Must accept at least two arguments: the fitted 
#' \code{object} and a \code{data.frame} \code{newdata} with data 
#' on which to predict the outcome.
#' @param pred.args (optional) Arguments to \code{pred.fun} (in addition to the 
#' fitted model object and the \code{newdata} argument, 
#' which are provided by \code{sperrorest})
#' @param smp.fun A function for sampling training and test sets from 
#' \code{data}. E.g., \code{\link{partition.kmeans}} for 
#' spatial cross-validation using spatial \emph{k}-means clustering.
#' @param smp.args (optional) Arguments to be passed to \code{est.fun}
#' @param train.fun (optional) A function for resampling or subsampling the 
#' training sample in order to achieve, e.g., uniform sample sizes on all 
#' training sets, or maintaining a certain ratio of positives and negatives 
#' in training sets. 
#' E.g., \code{\link{resample.uniform}} or \code{\link{resample.strat.uniform}}
#' @param train.param (optional) Arguments to be passed to \code{resample.fun}
#' @param test.fun (optional) Like \code{train.fun} but for the test set.
#' @param test.param (optional) Arguments to be passed to \code{test.fun}
#' @param err.fun A function that calculates selected error measures from the 
#' known responses in \code{data} and the model predictions delivered 
#' by \code{pred.fun}. E.g., \code{\link{err.default}} (the default). 
#' See example and details below.
#' @param err.fold logical (default: \code{TRUE} if \code{importance} is 
#' \code{TRUE}, otherwise \code{FALSE}): calculate error measures on each fold 
#' within a resampling repetition.
#' @param err.rep logical (default: \code{TRUE}): calculate error measures 
#' based on the pooled predictions of all folds within a resampling repetition.
#' @param err.train logical (default: \code{TRUE}): calculate error measures on 
#' the training set (in addition to the test set estimation).
#' @param imp.variables (optional; used if \code{importance=TRUE}) 
#' Variables for which permutation-based variable importance assessment 
#' is performed. If \code{importance=TRUE} and \code{imp.variables} is 
#' \code{NULL}, all variables in \code{formula} will be used.
#' @param imp.permutations (optional; used if \code{importance=TRUE}) 
#' Number of permutations used for variable importance assessment.
#' @param importance logical: perform permutation-based variable importance assessment?
#' @param ... currently not used
#' @param distance logical (default: \code{FALSE}): if \code{TRUE}, calculate 
#' mean nearest-neighbour distances from test samples to training samples using 
#' \code{\link{add.distance.represampling}}
#' @param do.gc numeric (default: 1): defines frequency of memory garbage 
#' collection by calling \code{\link{gc}}; if \code{<1}, no garbage collection; 
#' if \code{>=1}, run a \code{gc()} after each repetition; 
#' if \code{>=2}, after each fold
#' @param do.try logical (default: \code{FALSE}): if \code{TRUE} [untested!!], 
#' use \code{\link{try}} to robustify calls to \code{model.fun} and 
#' \code{err.fun}; use with caution!
#' @param silent numeric. If 1, only information on repetitions are reported. 
#' If 2, information on repetitions and folds are reported. Default to 1. 
#' Set to FALSE for no progress output.
#' @param progress character. Where to write the output. The default results 
#' in console output for Unix-Systems. For Windows, the default is to write to 
#' "sperrorest.progress.txt" located in the current working directory.
#' @param par.units numeric. How many cores should be used. 
#' Default to 1/2 of available cores.
#' @param benchmark logical (default: \code{FALSE}): if \code{TRUE}, 
#' perform benchmarking and return \code{sperrorestbenchmarks} object
#' 
#' @return A list (object of class \code{sperrorest}) with (up to) six components:
#' \item{error.rep}{a \code{sperrorestpoolederror} object containing 
#' predictive performances at the repetition level}
#' \item{error.fold}{a \code{sperroresterror} object containing predictive 
#' performances at the fold level}
#' \item{represampling}{a \code{\link{represampling}} object}
#' \item{importance}{a \code{sperrorestimportance} object containing 
#' permutation-based variable importances at the fold level}
#' \item{benchmarks}{a \code{sperrorestbenchmarks} object containing 
#' information on the system the code is running on, starting and 
#' finishing times, number of available CPU cores, parallelization mode, 
#' number of parallel units, and runtime performance}
#' \item{package.version}{Information about the \code{sperrorest} package
#' version}
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
#' @seealso \pkg{ipred}
#' @export
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo = slides ~ dem + slope + hcurv + vcurv + 
#'      log.carea + cslope
#' 
#' # Example of a classification tree fitted to this data:
#' library(rpart)
#' ctrl = rpart.control(cp = 0.005) # show the effects of overfitting
#' fit = rpart(fo, data = ecuador, control = ctrl)
#' par(xpd = TRUE)
#' plot(fit, compress = TRUE, main = "Stoyan's landslide data set")
#' text(fit, use.n = TRUE)
#'
#' # Non-spatial 50-repeated 5-fold cross-validation:
#' mypred.rpart = function(object, newdata) predict(object, newdata)[,2]
#' nspres = sperrorest.parallel(data = ecuador, formula = fo,
#'     model.fun = rpart, model.args = list(control = ctrl),
#'     pred.fun = mypred.rpart,
#'     smp.fun = partition.cv, 
#'     smp.args = list(repetition = 1:50, nfold = 5),
#'     par.units = 2, silent = 1,
#'     err.rep = TRUE, err.fold = TRUE)
#' summary(nspres$rep.error$train.auroc)     
#' summary(nspres$represampling)
#'
#' # Spatial 50-repeated 10-fold spatial cross-validation:
#' spres = sperrorest.parallel(data = ecuador, formula = fo,
#'     model.fun = rpart, model.args = list(control = ctrl),
#'     pred.fun = mypred.rpart,
#'     smp.fun = partition.cv, 
#'     smp.args = list(repetition = 1:50, nfold = 5),
#'     par.units = 2, silent = 1,
#'     err.rep = TRUE, err.fold = TRUE)
#' summary(nspres$rep.error$test.auroc)       
#' summary(spres$represampling)
#' 
#' Value <- c(nspres$rep.error$train.auroc, nspres$rep.error$test.auroc,
#'            spres$rep.error$train.auroc, spres$rep.error$test.auroc)
#' Model <- c(rep("nspres (train)", 50), rep("nspres (test)", 50), 
#'            rep("spres (train)", 50), rep("spres (test)", 50))
#' Type <- c(rep("Non-Spatial", 100), rep("Spatial", 100))
#' 
#' smry = data.frame(
#'   Value = Value,
#'   Model = factor(Model),
#'   Type = factor(Type))
#'
#' library(ggplot2)    
#' ggplot(smry, aes(Type, Value, fill = Model)) + 
#'   geom_boxplot(width = 0.7, outlier.size = 0.3, outlier.shape = 19) +
#'   guides(fill = guide_legend(title = "Model",
#'                              title.theme = element_text(face = "italic",
#'                                                         angle = -0, 
#'                                                         size = 12))) +
#'   ylab("AUROC") + xlab("")
sperrorest.par <- function(formula, data, coords = c("x", "y"), 
                           model.fun, model.args = list(), 
                           pred.fun = NULL, pred.args = list(), 
                           smp.fun = partition.loo, 
                           smp.args = list(), train.fun = NULL, 
                           train.param = NULL, test.fun = NULL, 
                           test.param = NULL, err.fun = err.default, 
                           err.fold = TRUE, err.rep = FALSE, 
                           err.train = TRUE, imp.variables = NULL, 
                           imp.permutations = 1000, 
                           importance = !is.null(imp.variables),
                           distance = FALSE, do.gc = 1, do.try = FALSE, 
                           silent = 1, par.units = detectCores()/2, 
                           progress = "", benchmark = FALSE,
                           ...) 
{
  
  #if benchmark = TRUE, start clock
  if (benchmark) start.time = Sys.time()
  
  if (missing(model.fun)) 
    stop("'model.fun' is a required argument")
  if (as.character(attr(terms(formula), "variables"))[3] == 
      "...") 
    stop("formula of the form lhs ~ ... not accepted by
         'sperrorest'\nspecify all predictor variables explicitly")
  stopifnot(is.function(model.fun))
  stopifnot(is.function(smp.fun))
  if (!is.null(train.fun)) 
    stopifnot(is.function(train.fun))
  if (!is.null(test.fun)) 
    stopifnot(is.function(test.fun))
  stopifnot(is.function(err.fun))
  if (importance) {
    if (!err.fold) {
      warning("'importance=TRUE' currently only supported 
              with 'err.fold=TRUE'.\nUsing 'importance=FALSE'")
      importance = FALSE
    }
    stopifnot(is.numeric(imp.permutations))
    if (!is.null(imp.variables)) 
      stopifnot(is.character(imp.variables))
  }
  stopifnot(is.character(coords))
  stopifnot(length(coords) == 2)
  if (importance & !err.fold) 
    stop("variable importance assessment currently only supported 
         at the unpooled level")
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
  dots.args <- list(...)
  if (length(dots.args) > 0) {
    if (any(names(dots.args) == "predfun")) 
      stop("sorry: argument names have changed; 
           'predfun' is now 'pred.fun'")
    if (any(names(dots.args) == "model")) 
      stop("sorry: argument names have changed; 
           'model' is now 'model.fun'")
    if (any(names(dots.args) == "err.combined")) 
      stop("sorry: argument names have changed; 
           'err.combined' is now 'err.rep'")
    if (any(names(dots.args) == "err.uncombined")) 
      stop("sorry: argument names have changed; 
           'err.uncombined' is now 'err.fold'")
    warning("'...' arguments currently not supported:\n
            use 'model.args' to pass list of additional 
            arguments to 'model.fun'")
  }
  if (par.units > parallel::detectCores()) {
    cat("More cores specificied than available. 
         Setting par.units to 1/2 of available cores.")
    par.units <- 0.5 * detectCores()
  }
  response <- as.character(attr(terms(formula), "variables"))[2]
  smp.args$data <- data
  smp.args$coords <- coords
  resamp <- do.call(smp.fun, args = smp.args)
  if (distance) 
    resamp <- add.distance(resamp, data, coords = coords, 
                           fun = mean)
  if (err.fold) {
    res <- lapply(resamp, unclass)
    class(res) <- "sperroresterror"
  }
  else {
    res <- NULL
  }
  rep.err <- NULL
  is.factor.prediction <- NULL
  impo <- NULL
  if (importance) {
    if (is.null(imp.variables)) {
      imp.variables = strsplit(as.character(formula)[3], 
                               " + ", fixed = TRUE)[[1]]
    }
    impo <- resamp
    imp.one.rep <- as.list(rep(NA, length(imp.variables)))
    names(imp.one.rep) <- imp.variables
    tmp <- as.list(rep(NA, imp.permutations))
    names(tmp) <- as.character(1:imp.permutations)
    for (vnm in imp.variables) {
      imp.one.rep[[vnm]] <- tmp
    }
    rm(tmp)
  }
  
  if (progress == "" & Sys.info()["sysname"] == "Windows") {
    progress <- paste0(getwd(), "/sperrorest.progress.txt")
  }
  
  cl <- makeCluster(par.units, outfile = progress)
  registerDoParallel(cl, cores = par.units)
  
  foreach.out <- foreach(i = 1:length(resamp), 
                         .packages = (.packages()), 
                         .errorhandling = "remove", 
                         .combine = rbind, .verbose = F) %dopar% {
                           
                           # reset rep.err otherwise 
                           # duplicates are introduced
                           rep.err <- NULL
                           
                           if (silent == 1) {
                             cat(date(), "Repetition", 
                                 names(resamp)[i], "\n") 
                           }
                           if (err.train) {
                             pooled.obs.train = pooled.pred.train = c()
                             pooled.obs.test = pooled.pred.test = c()
                           }
                           for (j in 1:length(resamp[[i]])) {
                             if (silent == 2) {
                               cat(date(), "Repetition", 
                                   names(resamp)[i], "- Fold", j, "\n") 
                             }
                             nd <- data[resamp[[i]][[j]]$train, ]
                             if (!is.null(train.fun)) {
                               nd <- train.fun(data = nd, param = train.param)
                             }
                             margs <- c(list(formula = formula, data = nd),
                                        model.args)
                             if (do.try) {
                               fit <- try(do.call(model.fun, args = margs),
                                          silent = silent)
                               if (class(fit) == "try-error") {
                                 fit <- NULL
                                 if (err.fold) {
                                   if (err.train) {
                                     res[[i]][[j]]$train <- NULL
                                   }
                                   res[[i]][[j]]$test <- NULL
                                   if (importance) {
                                     impo[[i]][[j]] = c()
                                   }
                                 }
                                 if (do.gc >= 2) {
                                   gc()
                                 }
                                 next
                               }
                             }
                             else {
                               fit <- do.call(model.fun, args = margs)
                             }
                             if (err.train) {
                               pargs <- c(list(object = fit, newdata = nd),
                                          pred.args)
                               if (is.null(pred.fun)) {
                                 pred.train <- do.call(predict, args = pargs)
                               }
                               else {
                                 pred.train <- do.call(pred.fun, 
                                                       args = pargs)
                               }
                               rm(pargs)
                               if (err.fold) 
                                 if (do.try) {
                                   err.try <- try(err.fun(nd[, response],
                                                          pred.train), 
                                                  silent = silent)
                                   if (class(err.try) == "try-error") 
                                     err.try <- NULL
                                   res[[i]][[j]]$train <- err.try
                                 }
                               else {
                                 res[[i]][[j]]$train <- err.fun(
                                   nd[, response], pred.train)
                               }
                               if (err.rep) {
                                 pooled.obs.train <- c(pooled.obs.train, 
                                                       nd[, response])
                                 pooled.pred.train <- c(pooled.pred.train,
                                                        pred.train)
                               }
                             }
                             else {
                               if (err.fold) {
                                 res[[i]][[j]]$train <- NULL
                               }
                             }
                             nd <- data[resamp[[i]][[j]]$test, ]
                             if (!is.null(test.fun)) {
                               nd <- test.fun(data = nd, param = test.param)
                             }
                             if (importance) {
                               nd.bak <- nd
                             }
                             pargs <- c(list(object = fit, newdata = nd),
                                        pred.args)
                             if (is.null(pred.fun)) {
                               pred.test <- do.call(predict, args = pargs)
                             }
                             else {
                               pred.test <- do.call(pred.fun, args = pargs)
                             }
                             rm(pargs)
                             if (err.fold) {
                               if (do.try) {
                                 err.try <- try(err.fun(
                                   nd[, response],pred.test), silent = silent)
                                 if (class(err.try) == "try-error") 
                                   err.try <- NULL
                                 res[[i]][[j]]$test <- err.try
                               }
                               else {
                                 res[[i]][[j]]$test <- err.fun(
                                   nd[, response], pred.test)
                               }
                             }
                             if (err.rep) {
                               pooled.obs.test <- c(pooled.obs.test, 
                                                    nd[, response])
                               pooled.pred.test <- c(pooled.pred.test,
                                                     pred.test)
                               is.factor.prediction <- is.factor(pred.test)
                             }
                             if (importance & err.fold) {
                               if (is.null(res[[i]][[j]]$test)) {
                                 impo[[i]][[j]] <- c()
                                 if (silent == 1 | silent == 2) 
                                   cat(date(), 
                                       "-- skipping variable importance\n")
                               }
                               else {
                                 if (silent == 1 | silent == 2) {
                                   cat(date(), "-- Variable importance\n")
                                 }
                                 imp.temp <- imp.one.rep
                                 for (cnt in 1:imp.permutations) {
                                   if (silent == 1 | silent == 2 & 
                                       (cnt > 1)) 
                                     if (log10(cnt) == floor(log10(cnt))) 
                                       cat(date(), "   ", cnt, "\n")
                                   permut <- sample(1:nrow(nd), 
                                                    replace = FALSE)
                                   for (vnm in imp.variables) {
                                     nd <- nd.bak
                                     nd[, vnm] <- nd[, vnm][permut]
                                     pargs <- c(list(object = fit, 
                                                     newdata = nd), 
                                                pred.args)
                                     if (is.null(pred.fun)) {
                                       pred.test <- do.call(predict, 
                                                            args = pargs)
                                     }
                                     else {
                                       pred.test <- do.call(pred.fun, 
                                                            args = pargs)
                                     }
                                     rm(pargs)
                                     if (do.try) {
                                       permut.err <- try(err.fun(
                                         nd[, response], 
                                         pred.test), silent = silent)
                                       if (class(permut.err) == "try-error") {
                                         imp.temp[[vnm]][[cnt]] = c()
                                       }
                                       else {
                                         imp.temp[[vnm]][[cnt]] <- as.list(
                                           unlist(
                                             res[[i]][[j]]$test) - 
                                             unlist(permut.err))
                                       }
                                     }
                                     else {
                                       permut.err <- err.fun(nd[, response], 
                                                             pred.test)
                                       imp.temp[[vnm]][[cnt]] <- as.list(
                                         unlist(res[[i]][[j]]$test) - 
                                           unlist(permut.err))
                                     }
                                   }
                                 }
                                 impo[[i]][[j]] <- as.data.frame(
                                   t(sapply(imp.temp, 
                                            function(y) sapply(
                                              as.data.frame(t(sapply(
                                                y, as.data.frame))), 
                                              function(x) mean(
                                                unlist(x))))))
                                 rm(nd.bak, nd)
                               }
                             }
                           } #end of each fold
                           if (err.rep) {
                             if (is.factor(data[, response])) {
                               lev <- levels(data[, response])
                               if (err.train) {
                                 pooled.obs.train <-
                                   factor(lev[pooled.obs.train], 
                                          levels = lev)
                                 pooled.obs.test <-
                                   factor(lev[pooled.obs.test], 
                                          levels = lev)
                               }
                               if (is.factor.prediction) {
                                 if (err.train) {
                                   pooled.pred.train <-
                                     factor(lev[pooled.pred.train], 
                                            levels = lev)
                                   pooled.pred.test <-
                                     factor(lev[pooled.pred.test], 
                                            levels = lev)
                                 }
                               }
                               rep.err.train <- NULL
                               if (err.train) {
                                 rep.err.train <-
                                   err.fun(pooled.obs.train, 
                                           pooled.pred.train)
                               }
                               if (i == 1) {
                                 rep.err <- t(unlist(list(
                                   train = rep.err.train, 
                                   test = err.fun(pooled.obs.test,
                                                  pooled.pred.test))))
                               }
                               else {
                                 rep.err <- rbind(rep.err,
                                                  unlist(list(
                                                    train =
                                                      rep.err.train, 
                                                    test = err.fun(
                                                      pooled.obs.test,
                                                      pooled.pred.test))))
                               }
                               if (do.gc >= 2) {
                                 gc()
                               }
                             }
                             if ((do.gc >= 1) & (do.gc < 2)) {
                               gc()
                             }
                           }
                           
                           if (err.rep & err.fold) {
                             foreach.out <- list(rep.err, res)
                             return(foreach.out)
                           }
                           if (err.rep & !err.fold) {
                             foreach.out <- rep.err
                             return(foreach.out)
                           }
                           if (!err.rep & err.fold) {
                             foreach.out <- res
                             return(foreach.out)
                           }
                         }
  stopCluster(cl)
  
  if (err.rep & !err.fold) {
    rep.err <- as.data.frame(foreach.out)
  }
  if (err.rep & err.fold) {
    for (i in 1:length(resamp)) {
      foreach.out[[i]] <- as.data.frame(foreach.out[[i]])
    }
    for (i in 2:length(resamp)) {
      foreach.out[[1]] <- merge.data.frame(foreach.out[[1]], 
                                           foreach.out[[i]], all = TRUE)
    }
    i <- 2
    while (i <= length(resamp)) {
      foreach.out[[2]] <- NULL
      i <- i + 1
    }
  }
  
  if (silent == 1 | silent == 2) {
    cat(date(), "Done.\n")
  }
  if (importance) {
    class(impo) <- "sperrorestimportance"
  }
  
  if (benchmark) {
    end.time = Sys.time()
    my.bench = list(system.info = Sys.info(),
                    t.start = start.time,
                    t.end = end.time,
                    cpu.cores = detectCores(),
                    par.mode = NA,
                    par.units = par.units,
                    runtime.performance = end.time - start.time)
    class(my.bench) = "sperrorestbenchmarks"
  }
  else my.bench = NULL
  
  if (err.rep & err.fold) {
    res <- foreach.out
    res[[1]] <- NULL
    class(res[[1]]) = "sperroresterror"
    class(foreach.out[[1]]) = "sperrorestpoolederror"
    RES <- list(error.rep = foreach.out[[1]],
                error.fold = res[[1]], 
                represampling = resamp, 
                importance = impo, 
                benchmarks = my.bench,
                package.version = packageVersion("sperrorest"))
    class(RES) <- "sperrorest"
    return(RES)
  }
  if (err.rep & !err.fold) {
    class(rep.err) = "sperrorestpoolederror"
    RES <- list(error.rep = rep.err,
                error.fold = NULL, 
                represampling = resamp, 
                importance = impo, 
                benchmarks = my.bench,
                package.version = packageVersion("sperrorest"))
    class(RES) <- "sperrorest"
    return(RES)
  }
  if (!err.rep & err.fold) {
    class(foreach.out) = "sperroresterror"
    RES <- list(error.rep = NULL,
                error.fold = foreach.out, 
                represampling = resamp, 
                importance = impo, 
                benchmarks = my.bench,
                package.version = packageVersion("sperrorest"))
    class(RES) <- "sperrorest"
    return(RES)
  }
}


