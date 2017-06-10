#' Perform spatial error estimation and variable importance assessment
#' in parallel
#'
#' `sperrorest` is a flexible interface for multiple types of
#' parallelized spatial and non-spatial cross-validation
#' and bootstrap error estimation and parallelized permutation-based
#' assessment of spatial variable importance.
#'
#' @inheritParams partition_cv
#'
#' @import pbapply
#' @import magrittr
#' @import pbmcapply
#' @import parallel
#' @import foreach
#' @import future
#' @import doFuture
#' @import rpart
#' @importFrom utils packageVersion
#' @importFrom purrr walk map
#' @importFrom stringr str_replace_all
#'
#' @param data a `data.frame` with predictor and response variables.
#' Training and test samples will be drawn from this data set by `train_fun`
#' and `test_fun`, respectively.
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
#' @param model_fun Function that fits a predictive model, such as `glm`
#' or `rpart`. The function must accept at least two arguments, the first
#' one being a formula and the second a data.frame with the learning sample.
#' @param model_args Arguments to be passed to `model_fun`
#' (in addition to the `formula` and `data` argument,
#' which are provided by `sperrorest`)
#'
#' @param pred_fun Prediction function for a fitted model object created
#' by `model`. Must accept at least two arguments: the fitted
#' `object` and a `data.frame` `newdata` with data
#' on which to predict the outcome.
#'
#' @param pred_args (optional) Arguments to `pred_fun` (in addition to the
#' fitted model object and the `newdata` argument,
#' which are provided by `sperrorest`)
#'
#' @param smp_fun A function for sampling training and test sets from
#' `data`. E.g. [partition_kmeans] for
#' spatial cross-validation using spatial \emph{k}-means clustering.
#'
#' @param smp_args (optional) Arguments to be passed to `smp_fun`.
#'
#' @param train_fun (optional) A function for resampling or subsampling the
#' training sample in order to achieve, e.g., uniform sample sizes on all
#' training sets, or maintaining a certain ratio of positives and negatives
#' in training sets.
#' E.g. [resample_uniform] or [resample_strat_uniform].
#'
#' @param train_param (optional) Arguments to be passed to `resample_fun`
#'
#' @param test_fun (optional) Like `train_fun` but for the test set.
#'
#' @param test_param (optional) Arguments to be passed to `test_fun`
#'
#' @param err_fun A function that calculates selected error measures from the
#' known responses in `data` and the model predictions delivered
#' by `pred_fun`. E.g. [err_default] (the default).
#'
#' @param imp_variables (optional; used if `importance = TRUE`).
#' Variables for which permutation-based variable importance assessment
#' is performed. If `importance = TRUE` and `imp_variables` ==
#' `NULL`, all variables in `formula` will be used.
#'
#' @param imp_permutations (optional; used if `importance = TRUE`).
#' Number of permutations used for variable importance assessment.
#'
#' @param importance logical (default: `FALSE`): perform permutation-based
#' variable importance assessment?
#'
#' @param distance logical (default: `FALSE`): if `TRUE`, calculate
#' mean nearest-neighbour distances from test samples to training samples using
#' [add.distance.represampling]
#'
#' @param do_gc numeric (default: 1): defines frequency of memory garbage
#' collection by calling [gc]; if `< 1`, no garbage collection;
#' if `>= 1`, run a [gc] after each repetition;
#' if `>= 2`, after each fold
#'
#' @param progress numeric (default: `1`): Whether to show progress
#' information (if possible). Default shows repetition and fold progress on
#' `par_mode = "foreach"` or `par_mode = "sequential"`.
#' Set to `FALSE` for no progress information.
#'
#' @param out_progress only used if `par_mode = foreach`: Write progress
#' output to a file instead of console output.
#' The default (`''`) results in console output for Unix-systems and
#' file output ('sperrorest.progress.txt') in the current working directory
#' for Windows systems. No console output is possible on Windows systems.
#'
#' @param par_args list of parallelization parameters:
#' \itemize{
#' \item{`par_mode`:} {the parallelization mode. See details.}
#' \item{`par_units`:} {the number of parallel processing units.}
#' \item{`par_option`:} {optional [future] settings for `par_mode = "future"` or
#' `par_mode = "foreach"`.}
#' }
#'
#' @param benchmark (optional) logical (default: `FALSE`): if `TRUE`,
#' perform benchmarking and return `sperrorestbenchmark` object
#'
#' @param ... Further options passed to [makeCluster] for
#' `par_mode = "foreach"`.
#'
#' @return A list (object of class `sperrorest`) with (up to) six components:
#' \item{error_rep}{a `sperrorestreperror` object containing
#' predictive performances at the repetition level}
#' \item{error_fold}{a `sperroresterror` object containing predictive
#' performances at the fold level}
#' \item{represampling}{a [represampling()] object}
#' \item{importance}{a `sperrorestimportance` object containing
#' permutation-based variable importances at the fold level}
#' \item{benchmark}{a `sperrorestbenchmark` object containing
#' information on the system the code is running on, starting and
#' finishing times, number of available CPU cores, parallelization mode,
#' number of parallel units, and runtime performance}
#' \item{package_version}{a `sperrorestpackageversion` object containing
#' information about the `sperrorest` package version}
#'
#' @details By default `sperrorest` runs in parallel on all cores using
#' `foreach` with the [future] backend. If this is not desired, specify
#' `par_units` in `par_args` or set `par_mode = "sequential"`.
#'
#' Available parallelization modes include `par_mode = "apply"`
#' (calls [pbmclapply] on Unix, [parApply] on Windows) and
#' `future` ([future_lapply]).
#' For the latter and `par_mode = "foreach"`, `par_option`
#' (default to `multiprocess` and
#' `cluster`, respectively) can be specified. See [plan] for further details.
#'
#' @note Custom predict functions passed to `pred_fun`, which consist of
#' multiple custom defined child functions, must be defined in one function.
#'
#' @references Brenning, A. 2012. Spatial cross-validation and bootstrap for
#' the assessment of prediction rules in remote sensing: the R package
#' 'sperrorest'.
#' 2012 IEEE International Geoscience and Remote Sensing Symposium (IGARSS),
#' 23-27 July 2012, p. 5372-5375.
#'
#' Brenning, A. 2005. Spatial prediction models for landslide hazards: review,
#' comparison and evaluation. Natural Hazards and Earth System Sciences,
#' 5(6): 853-862.
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
#'
#' ##------------------------------------------------------------
#' ## Classification tree example using non-spatial partitioning
#' ## setup and default parallel mode ("foreach")
#' ##------------------------------------------------------------
#'
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' library(rpart)
#' mypred_part <- function(object, newdata) predict(object, newdata)[, 2]
#' ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
#' fit <- rpart(fo, data = ecuador, control = ctrl)
#'
#' ### Non-spatial 5-repeated 10-fold cross-validation:
#' mypred_part <- function(object, newdata) predict(object, newdata)[, 2]
#' par_nsp_res <- sperrorest(data = ecuador, formula = fo,
#'                           model_fun = rpart,
#'                           model_args = list(control = ctrl),
#'                           pred_fun = mypred_part,
#'                           progress = TRUE,
#'                           smp_fun = partition_cv,
#'                           smp_args = list(repetition = 1:5, nfold = 10))
#' summary(par_nsp_res$error_rep)
#' summary(par_nsp_res$error_fold)
#' summary(par_nsp_res$represampling)
#' # plot(par_nsp_res$represampling, ecuador)
#'
#' ### Spatial 5-repeated 10-fold spatial cross-validation:
#' par_sp_res <- sperrorest(data = ecuador, formula = fo,
#'                          model_fun = rpart,
#'                          model_args = list(control = ctrl),
#'                          pred_fun = mypred_part,
#'                          progress = TRUE,
#'                          smp_fun = partition_kmeans,
#'                          smp_args = list(repetition = 1:5, nfold = 10))
#' summary(par_sp_res$error_rep)
#' summary(par_sp_res$error_fold)
#' summary(par_sp_res$represampling)
#' # plot(par_sp_res$represampling, ecuador)
#'
#' smry <- data.frame(
#'     nonspat_training = unlist(summary(par_nsp_res$error_rep,
#'                                       level = 1)$train_auroc),
#'     nonspat_test     = unlist(summary(par_nsp_res$error_rep,
#'                                       level = 1)$test_auroc),
#'     spatial_training = unlist(summary(par_sp_res$error_rep,
#'                                       level = 1)$train_auroc),
#'     spatial_test     = unlist(summary(par_sp_res$error_rep,
#'                                      level = 1)$test_auroc))
#' boxplot(smry, col = c('red','red','red','green'),
#'     main = 'Training vs. test, nonspatial vs. spatial',
#'     ylab = 'Area under the ROC curve')
#'
#' ##------------------------------------------------------------
#' ## Logistic regression example (glm) using partition_kmeans
#' ## and computation of permutation based variable importance
#' ##------------------------------------------------------------
#'
#' data(ecuador)
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sperrorest(data = ecuador, formula = fo,
#'                      model_fun = glm,
#'                      model_args = list(family = "binomial"),
#'                      pred_fun = predict,
#'                      pred_args = list(type = "response"),
#'                      smp_fun = partition_cv,
#'                      smp_args = list(repetition = 1:2, nfold = 4),
#'                      par_args = list(par_mode = "future"),
#'                      importance = TRUE, imp_permutations = 10)
#' summary(out$error_rep)
#' summary(out$importance)
#' }
#' @export
sperrorest <- function(formula, data, coords = c("x", "y"),
                       model_fun, model_args = list(),
                       pred_fun = NULL, pred_args = list(),
                       smp_fun = partition_cv, smp_args = list(),
                       train_fun = NULL, train_param = NULL, test_fun = NULL,
                       test_param = NULL, err_fun = err_default,
                       imp_variables = NULL,
                       imp_permutations = 1000,
                       importance = !is.null(imp_variables), distance = FALSE,
                       par_args = list(par_mode = "foreach", par_units = NULL,
                                       par_option = NULL),
                       do_gc = 1, progress = 1,
                       out_progress = "", benchmark = FALSE, ...) {
  # if benchmark = TRUE, start clock
  if (benchmark)
    start_time <- Sys.time()

  # Some checks:
  if (missing(model_fun))
    stop("'model_fun' is a required argument")
  if (as.character(attr(terms(formula), "variables"))[3] == "...")
    stop("formula of the form lhs ~ ... not accepted by 'sperrorest'\n
         specify all predictor variables explicitly")
  stopifnot(is.function(model_fun))
  stopifnot(is.function(smp_fun))
  if (!is.null(train_fun))
    stopifnot(is.function(train_fun))
  if (!is.null(test_fun))
    stopifnot(is.function(test_fun))
  stopifnot(is.function(err_fun))

  if (importance) {
    if (!error_fold) {
      warning(paste0("'importance = TRUE' currently only supported with",
                     " 'error_fold = TRUE'.\n", "Using 'importance = FALSE'"))
      importance <- FALSE
    }

    stopifnot(is.numeric(imp_permutations))

    if (!is.null(imp_variables)) {
      stopifnot(is.character(imp_variables)) # nocov
    }
  }
  stopifnot(is.character(coords))
  stopifnot(length(coords) == 2)

  if (importance & !error_fold) {
    stop(paste0("variable importance assessment currently only supported", # nocov
                " at the unpooled level")) # nocov
  }

  # Check if user is trying to bypass the normal mechanism for
  # generating training and test data sets and for passing formulas:
  if (any(names(model_args) == "formula"))
    stop("'model_args' cannot have a 'formula' element")
  if (any(names(model_args) == "data"))
    stop("'model_args' cannot have a 'data' element")
  if (any(names(pred_args) == "object"))
    stop("'pred_args' cannot have an 'object' element:\n
         this will be generated by 'sperrorest'")
  if (any(names(pred_args) == "newdata"))
    stop("'pred_args' cannot have a 'newdata' element:\n
         this will be generated by 'sperrorest'")

  # Deprecated argumentsSun May 21 21:31:51 2017 ------------------------------
  dots_args <- list(...)
  if (length(dots_args) > 0) {
    if (any(names(dots_args) == "predfun")) {
      stop("argument names have changed; 'predfun' is now 'pred_fun'")
    }
    if (any(names(dots_args) == "silent")) {
      stop("argument names have changed; 'silent' is now 'progress'")
    }
    if (any(names(dots_args) == "err.pooled")) {
      stop(paste0("argument names have changed; 'err.pooled' is now",
                  " 'error_rep'"))
    }
    if (any(names(dots_args) == "err.unpooled")) {
      stop(paste0("argument names have changed; 'err.unpooled' is now",
                  "'error_fold'"))
    }
    # > v2.0.0
    if (class(par_args$par_mode) == "numeric") { # nocov start
      stop("'par_mode' has to be specified using an explicit parallel mode name.")
    }
    if (any(names(dots_args) == "error.fold") |
        any(names(dots_args) == "error.rep") |
        any(names(dots_args) == "error.train")) {
      stop(paste0("'error.fold', 'error.rep' and 'err.train' are
                  deprecated and calculated by default now. "))
    } # nocov end
  }

  # Name of response variable:
  response <- as.character(attr(terms(formula), "variables"))[2]

  smp_args$data <- data
  smp_args$coords <- coords

  resamp <- do.call(smp_fun, args = smp_args)

  if (distance)
    # Parallelize this function???
    resamp <- add.distance(resamp, data, coords = coords, fun = mean)

  res <- lapply(resamp, unclass)
  class(res) <- "sperroresterror"

  pooled_error <- NULL

  # required to be able to assign levels to predictions if appropriate:
  is_factor_prediction <- NULL

  ### Permutation-based variable importance assessment (optional):
  impo <- NULL
  if (importance) {
    # Importance of which variables:
    if (is.null(imp_variables)) {
      imp_variables <- strsplit(as.character(formula)[3], " + ",
                                fixed = TRUE)[[1]]
    }
    # Dummy data structure that will later be populated with the results:
    impo <- resamp
    # Create a template that will contain results of variable importance
    # assessment:
    rep(NA, length(imp_variables)) %>%
      as.list() -> imp_one_rep

    names(imp_one_rep) <- imp_variables

    rep(NA, imp_permutations) %>%
      as.list() -> tmp

    names(tmp) <- as.character(1:imp_permutations)
    for (vnm in imp_variables) {
      imp_one_rep[[vnm]] <- tmp
    }
    rm(tmp)
  }

  # prevent unnecessary starts of too many workers
  if (is.null(par_args$par_units) && length(resamp) < availableCores() &&
      !par_args$par_mode == "sequential") {
    par_args$par_units <- length(resamp)
    message(sprintf(paste0("Setting number of cores equal to repetition count",
                           " (= %s) to avoid starting unnecessary workers."),
                    length(resamp)))
  } else if (!is.null(par_args$par_units) &&
             length(resamp) < par_args$par_units &&
             !par_args$par_mode == "sequential") {
    par_args$par_units <- length(resamp)
    message(sprintf(paste0("Setting number of cores equal to repetition count",
                           " (= %s) to avoid starting unnecessary workers."),
                    length(resamp)))
  }

  # account for unspecified number of cores
  if (is.null(par_args$par_units)) {
    par_args$par_units <- availableCores()
  }

  ### par_mode = "apply" (pbapply) -------

  if (par_args$par_mode == "apply" | par_args$par_mode == "future" |
      par_args$par_mode == "apply-mclapply") {

    if (par_args$par_units > availableCores()) {
      par_args$par_units <- availableCores() # nolint # nocov
    }


    # parallelization here (par_mode = 1 & par_mode = 2) For each repetition:
    if (.Platform$OS.type == "windows") {
      par_cl <- makeCluster(par_args$par_units, type = "PSOCK") # nocov start
      clusterSetRNGStream(par_cl, 1234567)  #set up RNG stream to obtain
      # reproducible results
      force(pred_fun)  #force evaluation of pred_fun, so it is serialized and
      # provided to all cluster workers
      clusterExport(par_cl, "par_args", envir = environment())
      # clusterEvalQ(par_cl, {
      #   map(X = par_args$par.libs, function(n)
      #   {
      #     do.call("library", list(n))
      #   })
      #   NULL
      # })
    } else { # nocov end
      RNGkind("L'Ecuyer-CMRG")
      set.seed(1234567)
      # mc.reset.stream() #set up RNG stream to obtain reproducible results
      # comment: causing build to fail on Windows. Not sure if really necessary
      # -> not in use for now
      par_cl <- par_args$par_units
    }

    # parLapply is called when cl is a 'cluster' object, mclapply is called
    # when cl is integer. Showing the progress bar increases the communication
    # overhead between the main process and nodes / child processes compared to
    # the parallel equivalents of the functions without the progress bar.
    # see ?pblapply
    if (progress == FALSE) {
      pboptions(type = "none") # nolint
    } else {
      pboptions(style = 1, type = "timer") # nolint
    }

    #environment(runreps) <- environment()

    # runreps call Sun Apr  9 13:28:31 2017 ------------------------------

    if (par_args$par_mode == "apply" | par_args$par_mode == "apply-mclapply") {
      if (.Platform$OS.type == "Windows") {

        message(sprintf("Using 'parApply' parallel mode with %s cores.", # nocov start
                        par_args$par_units))
        my_res <- try(pblapply(cl = par_cl, resamp, function(x)
          runreps(current_sample = x, data = data, par_mode = par_args$par_mode,
                  formula = formula, do_gc = do_gc, imp_one_rep = imp_one_rep,
                  pred_fun = pred_fun,
                  model_args = model_args,
                  model_fun = model_fun,
                  imp_permutations = imp_permutations,
                  imp_variables = imp_variables,
                  is_factor_prediction = is_factor_prediction,
                  importance = importance,
                  current_res = current_res,
                  pred_args = pred_args, response = response, par_cl = par_cl,
                  coords = coords, progress = progress,
                  pooled_obs_train = pooled_obs_train,
                  pooled_obs_test = pooled_obs_test, err_fun = err_fun)))
      } else {
        # not provided as an option to the user -> pbmclapply is faster
        # only used for performance evaluation
        if (par_args$par_mode == "apply-mclapply") {
          message(sprintf("Using 'mclapply' parallel mode with %s cores.",
                          par_args$par_units))
          my_res <- try(mclapply(mc.cores = par_cl, resamp, function(x)
            runreps(current_sample = x, data = data,
                    par_mode = par_args$par_mode,
                    formula = formula, do_gc = do_gc, imp_one_rep = imp_one_rep,
                    pred_fun = pred_fun,
                    model_args = model_args,
                    model_fun = model_fun,
                    imp_permutations = imp_permutations,
                    imp_variables = imp_variables,
                    is_factor_prediction = is_factor_prediction,
                    importance = importance,
                    current_res = current_res,
                    pred_args = pred_args, response = response, par_cl = par_cl,
                    coords = coords, progress = progress,
                    pooled_obs_train = pooled_obs_train,
                    pooled_obs_test = pooled_obs_test, err_fun = err_fun)))
          if (my_res == "NULL") {
            stop(paste0("No output was received from sperrorest.\n",
                        "If you are on macOS either run R in 'Vanilla' mode or",
                        " use another parallel mode."))
          } # nocov end
        } else {
          message(sprintf("Using 'pbmclapply' parallel mode with %s cores.",
                          par_args$par_units))
          my_res <- try(pbmclapply(mc.cores = par_cl, resamp, function(x)
            runreps(current_sample = x, data = data,
                    par_mode = par_args$par_mode,
                    formula = formula, do_gc = do_gc, imp_one_rep = imp_one_rep,
                    pred_fun = pred_fun,
                    model_args = model_args,
                    model_fun = model_fun,
                    imp_permutations = imp_permutations,
                    imp_variables = imp_variables,
                    is_factor_prediction = is_factor_prediction,
                    importance = importance,
                    current_res = current_res,
                    pred_args = pred_args, response = response, par_cl = par_cl,
                    coords = coords, progress = progress,
                    pooled_obs_train = pooled_obs_train,
                    pooled_obs_test = pooled_obs_test, err_fun = err_fun)))
          # check if run was sufficient
          if (length(my_res) > 1 && my_res == "NULL") {
            stop(paste0("No output was received from sperrorest.\n", # nocov start
                        "If you are on macOS either run R in 'Vanilla' mode or",
                        " use another parallel mode.")) # nocov end
          }
        }
      }
    }

    # par_mode = "future" Sun May 21 12:04:55 2017 -----------------------------

    if (par_args$par_mode == "future") {
      if (!is.null(par_args$par_option)) {
        plan(par_args$par_option, workers = par_args$par_units) # nocov
      } else {
        par_args$par_option <- "multiprocess"
        plan(par_args$par_option, workers = par_args$par_units)
      }

      message(sprintf("Using parallel framework 'future' with 'future_lapply'",
                      "and '%s' option.", par_args$par_option))
      my_res <- try(future_lapply(resamp, function(x)
        runreps(current_sample = x, data = data, par_mode = par_args$par_mode,
                formula = formula, do_gc = do_gc, imp_one_rep = imp_one_rep,
                pred_fun = pred_fun,
                model_args = model_args, model_fun = model_fun,
                imp_permutations = imp_permutations,
                imp_variables = imp_variables,
                is_factor_prediction = is_factor_prediction,
                importance = importance,
                current_res = current_res,
                pred_args = pred_args, response = response, par_cl = par_cl,
                coords = coords, progress = progress,
                pooled_obs_train = pooled_obs_train,
                pooled_obs_test = pooled_obs_test, err_fun = err_fun)))
    }
  }

  ### par_mode = "foreach" -------

  if (par_args$par_mode == "foreach" | par_args$par_mode == "sequential" |
      par_args$par_mode == "foreach-old") {

    # combine function for multiple object outputs in foreach call
    comb <- function(...) {
      mapply("rbind", ..., SIMPLIFY = FALSE)
    }

    # suppress any progress output of workes if progress = FALSE
    if (progress == FALSE) {
      out_progress <- "/dev/null"
      if (Sys.info()["sysname"] == "Windows") {
        out_progress <- "nul:" # nocov
      }
    }
    # special settings for Windows
    if (out_progress == "" & Sys.info()["sysname"] == "Windows") {
      out_progress <- paste0(getwd(), "/sperrorest_progress.txt") # nocov
    }

    registerDoFuture()

    par_args$par_option <- "cluster"

    # check for sequential/parallel execution and (if parallel) get number of
    # cores
    if (!par_args$par_mode == "sequential" &&
        par_args$par_mode == "foreach") {

      cl <- makeCluster(par_args$par_units, outfile = out_progress, ...)
      plan(cluster, workers = cl)

      message(sprintf(paste0("Using 'foreach' parallel mode with %s cores and",
                             " '%s' option."),
                      par_args$par_units, par_args$par_option))
      if (.Platform$OS.type == "Windows") {
        message(paste0("Please see 'sperrorest_progress' in your current", # nocov
                       " working directory for progress output on Windows.")) # nocov
      }
    }
    if (par_args$par_mode == "sequential") {
      registerDoFuture()
      plan(sequential)
      message(sprintf("Using 'foreach' sequential mode."))
    }

    # runreps call Fri May 19 14:35:58 2017 ------------------------------

    my_res <- foreach(i = 1:length(resamp), .packages = (.packages()),
                      .errorhandling = "remove", .combine = "comb",
                      .multicombine = TRUE, .verbose = FALSE) %dopar% {

                        pooled_obs_train <- pooled_pred_train <- c()
                        pooled_obs_test <- pooled_pred_test <- c()

                        current_res <- NULL
                        current_impo <- resamp[[i]]
                        current_pooled_error <- NULL

                        current_res <- map(resamp[[i]], unclass)
                        class(current_res) <- "sperroresterror"

                        environment(runfolds) <- environment()

                        if (progress == 2) {
                          cat(date(), "Repetition", i, "\n")
                        }

                        try(map(seq_along(resamp[[i]]), function(rep)
                          runfolds(j = rep, data = data,
                                   current_sample = resamp[[i]],
                                   formula = formula,
                                   par_mode = par_args$par_mode, i = i,
                                   imp_one_rep = imp_one_rep,
                                   pred_fun = pred_fun,
                                   model_args = model_args,
                                   model_fun = model_fun,
                                   imp_permutations = imp_permutations,
                                   imp_variables = imp_variables,
                                   is_factor_prediction = is_factor_prediction,
                                   importance = importance,
                                   current_res = current_res,
                                   pred_args = pred_args, response = response,
                                   par_cl = par_cl,
                                   coords = coords, progress = progress,
                                   pooled_obs_train = pooled_obs_train,
                                   pooled_obs_test = pooled_obs_test,
                                   err_fun = err_fun))) -> runfolds_list

                        # merge sublists of each fold into one list
                        # http://stackoverflow.com/questions/32557131/adding-a-vector-to-each-sublist-within-a-list-r # nolint
                        # http://stackoverflow.com/questions/43963683/r-flexible-passing-of-sublists-to-following-function # nolint
                        runfolds_merged <- do.call(Map, c(f = list,
                                                          runfolds_list))


                        if (importance == TRUE) {
                          # subset fold result to importance results only
                          impo_only <- runfolds_merged[6][[1]]
                        }

                        pooled_only <- runfolds_merged[c(1:4)]
                        pooled_only <- sapply(unique(names(pooled_only)),
                                              function(x) unname(unlist(
                                                pooled_only[
                                                  names(pooled_only) == x])),
                                              simplify = FALSE)

                        # Put the results from the pooled estimation into the
                        # pooled_error data structure:
                        if (is.factor(data[, response])) {
                          lev <- levels(data[, response])

                          # err_train
                          pooled_only$pooled_obs_train <- factor(lev[
                            pooled_only$pooled_obs_train], levels = lev)

                          pooled_only$pooled_obs_test <- factor(lev[
                            pooled_only$pooled_obs_test], levels = lev)
                          if (is_factor_prediction) {
                            if (err_train) {
                              pooled_only$pooled_pred_train <- factor(lev[
                                pooled_only$pooled_pred_train], levels = lev)
                            }
                            pooled_only$pooled_pred_test <- factor(lev[
                              pooled_only$pooled_pred_test], levels = lev)
                          }
                        }
                        pooled_error_train <- NULL

                        # err_train
                        pooled_error_train <- err_fun(
                          pooled_only$pooled_obs_train,
                          pooled_only$pooled_pred_train)


                        current_pooled_error <- t(unlist(list(
                          train = pooled_error_train,
                          test = err_fun(pooled_only$pooled_obs_test,
                                         pooled_only$pooled_pred_test))))
                        current_pooled_error %>%
                          colnames() %>%
                          str_replace_all("[.]", "_") -> names
                        colnames(current_pooled_error) <- names

                        if (do_gc >= 2) {
                          gc()
                        }


                        if ((do_gc >= 1) & (do_gc < 2)) { # nolint
                          gc()
                        }

                        # set current_impo to NULL to prevent false importance
                        # output (resamp object)
                        if (importance == FALSE) {
                          impo_only <- NULL
                        }

                        result <- list(error = runfolds_merged$current_res,
                                       pooled_error = current_pooled_error,
                                       importance = impo_only)
                        return(list(result))
                      }
    if (par_args$par_mode == "foreach") {
      on.exit(stopCluster(cl))
    }
  }

  ### format parallel outputs ----

  if (par_args$par_mode == "foreach" | par_args$par_mode == "sequential") {
    # split combined lists from foreach output into sublists referring
    # to repetitions
    my_res <- split(my_res[[1]], 1:length(resamp))
  }

  # assign names to sublists - otherwise `transfer_parallel_output` doesn't work
  for (i in 1:length(my_res)) {
    names(my_res[[i]]) <- c("error", "pooled_error", "importance")
  }

  # transfer results of lapply() to respective data objects
  my_res_mod <- transfer_parallel_output(my_res, res, impo, pooled_error)

  pooled_error <- as.data.frame(my_res_mod$pooled_error)
  rownames(pooled_error) <- NULL
  class(pooled_error) <- "sperrorestreperror"

  if (importance) {
    impo <- my_res_mod$impo
    class(impo) <- "sperrorestimportance"
  }

  if (benchmark) {
    end_time <- Sys.time()
    my_bench <- list(system.info = Sys.info(), t_start = start_time,
                     t_end = end_time, cpu_cores = detectCores(),
                     par_mode = par_args$par_mode,
                     par_units = par_args$par_units,
                     runtime_performance = end_time - start_time)
    class(my_bench) <- "sperrorestbenchmark"
  } else {
    my_bench <- NULL
  }

  package_version <- packageVersion("sperrorest")
  class(package_version) <- "sperrorestpackageversion"

  res <- list(error_rep = pooled_error, error_fold = my_res_mod$res,
              represampling = resamp, importance = impo, benchmark = my_bench,
              package_version = package_version)
  class(res) <- "sperrorest"

  return(res)
}
