#' @title Perform spatial error estimation and variable importance assessment
#'
#' @description {sperrorest} is a flexible interface for multiple types of
#' parallelized spatial and non-spatial cross-validation and bootstrap error
#' estimation and parallelized permutation-based assessment of spatial variable
#' importance.
#'
#' @details Custom predict functions passed to `pred_fun`, which consist of
#'   multiple child functions, must be defined in one function.
#'
#' @section Parallelization:
#'
#' Running in parallel is supported via package \CRANpkg{future}.
#' Have a look at `vignette("future-1-overview", package = "future")`.
#' In short: Choose a backend and specify the number of workers, then call
#' `sperrorest()` as usual. Example:
#'
#' ```r
#' future::plan(future.callr::callr, workers = 2)
#' sperrorest()
#' ```
#' Parallelization at the repetition is recommended when using
#' repeated cross-validation. If the 'granularity' of parallelized
#' function calls is too fine, the overall runtime will be very
#' poor since the overhead for passing arguments and handling
#' environments becomes too large. Use fold-level parallelization
#' only when the processing time of individual folds is very
#' large and the number of repetitions is small or equals 1.
#'
#' Note that nested calls to `future` are not possible.
#' Therefore a sequential `sperrorest` call should be used for
#' hyperparameter tuning in a nested cross-validation.
#'
#' @importFrom future.apply future_lapply
#' @importFrom utils packageVersion tail
#' @importFrom stringr str_replace_all
#'
#' @param data a `data.frame` with predictor and response variables. Training
#'   and test samples will be drawn from this data set by `train_fun` and
#'   `test_fun`, respectively.
#'
#' @param formula A formula specifying the variables used by the `model`. Only
#'   simple formulas without interactions or nonlinear terms should be used,
#'   e.g. `y~x1+x2+x3` but not `y~x1*x2+log(x3)`. Formulas involving interaction
#'   and nonlinear terms may possibly work for error estimation but not for
#'   variable importance assessment, but should be used with caution.
#'   The formula `y~...` is not supported, but `y~1` (i.e. no predictors) is.
#' @param coords vector of length 2 defining the variables in `data` that
#'   contain the x and y coordinates of sample locations.
#' @param model_fun Function that fits a predictive model, such as `glm` or
#'   `rpart`. The function must accept at least two arguments, the first one
#'   being a formula and the second a data.frame with the learning sample.
#' @param model_args Arguments to be passed to `model_fun` (in addition to the
#'   `formula` and `data` argument, which are provided by {sperrorest})
#' @param pred_fun Prediction function for a fitted model object created by
#'   `model`. Must accept at least two arguments: the fitted `object` and a
#'   `data.frame` `newdata` with data on which to predict the outcome.
#' @param pred_args (optional) Arguments to `pred_fun` (in addition to the
#'   fitted model object and the `newdata` argument, which are provided by
#'   {sperrorest}).
#' @param smp_fun A function for sampling training and test sets from `data`.
#'   E.g. [partition_kmeans] for spatial cross-validation using spatial
#'   *k*-means clustering.
#' @param smp_args (optional) Arguments to be passed to `smp_fun`.
#' @param train_fun (optional) A function for resampling or subsampling the
#'   training sample in order to achieve, e.g., uniform sample sizes on all
#'   training sets, or maintaining a certain ratio of positives and negatives in
#'   training sets. E.g. [resample_uniform] or [resample_strat_uniform].
#' @param train_param (optional) Arguments to be passed to `resample_fun`.
#' @param test_fun (optional) Like `train_fun` but for the test set.
#' @param test_param (optional) Arguments to be passed to `test_fun`.
#' @param err_fun A function that calculates selected error measures from the
#'   known responses in `data` and the model predictions delivered by
#'   `pred_fun`. E.g. [err_default] (the default).
#' @param imp_variables (optional; used if `importance = TRUE`). Variables for
#'   which permutation-based variable importance assessment is performed. If
#'   `importance = TRUE` and `imp_variables` == `NULL`, all variables in
#'   `formula` will be used.
#' @param imp_sample_from (default: `"test"`): specified if the permuted feature
#'   values should be taken from the test set, the training set (a rather unlikely
#'   choice), or the entire sample (`"all"`). The latter is useful in
#'   leave-one-out resampling situations where the test set is simply too small
#'   to perform any kind of resampling. In any case importances are
#'   always estimates on the test set. (Note that resampling with replacement is
#'   used if the test set is larger than the set from which the permuted values
#'   are to be taken.)
#' @param imp_permutations (optional; used if `importance = TRUE`). Number of
#'   permutations used for variable importance assessment.
#' @param importance logical (default: `FALSE`): perform permutation-based
#'   variable importance assessment?
#' @param distance logical (default: `FALSE`): if `TRUE`, calculate mean
#'   nearest-neighbour distances from test samples to training samples using
#'   [add.distance.represampling].
#' @param do_gc numeric (default: 1): defines frequency of memory garbage
#'   collection by calling [gc]; if `< 1`, no garbage collection; if `>= 1`, run
#'   a [gc] after each repetition; if `>= 2`, after each fold.
#' @param progress character (default: `all`): Whether to show progress
#'   information (if possible). Default shows repetition, fold and (if enabled)
#'   variable importance progress. Set to `"rep"` for repetition information
#'   only or `FALSE` for no progress information.
#' @param mode_rep,mode_fold character (default: `"future"` and `"sequential"`,
#'   respectively): specifies whether to parallelize the execution at the repetition
#'   level, at the fold level, or not at all.
#'   Parallel execution uses `future.apply::future_lapply()` (see details below).
#'   It is only possible to parallelize at the repetition level or at
#'   the fold level.
#'   The `"loop"` option uses a `for` loop instead of an `lappy`
#'   function; this option is for debugging purposes.
#' @param benchmark (optional) logical (default: `FALSE`): if `TRUE`, perform
#'   benchmarking and return `sperrorestbenchmark` object.
#' @param verbose Controls the amount of information printed while processing.
#'   Defaults to 0 (no output).
#'
#' @return A list (object of class {sperrorest}) with (up to) six components:
#' - error_rep: `sperrorestreperror` containing
#' predictive performances at the repetition level
#' - error_fold: `sperroresterror` object containing predictive
#' performances at the fold level
#' - represampling: [represampling] object
#' - importance: `sperrorestimportance` object containing
#' permutation-based variable importances at the fold level
#' - benchmark: `sperrorestbenchmark` object containing
#' information on the system the code is running on, starting and
#' finishing times, number of available CPU cores and runtime performance
#' - package_version: `sperrorestpackageversion` object containing
#' information about the {sperrorest} package version
#'
#' @references Brenning, A. 2012. Spatial cross-validation and bootstrap for
#' the assessment of prediction rules in remote sensing: the R package
#' 'sperrorest'.
#' 2012 IEEE International Geoscience and Remote Sensing Symposium (IGARSS),
#' 23-27 July 2012, p. 5372-5375.
#' <https://ieeexplore.ieee.org/document/6352393>
#'
#' Brenning, A. 2005. Spatial prediction models for landslide hazards: review,
#' comparison and evaluation. Natural Hazards and Earth System Sciences,
#' 5(6), 853-862. <https://doi.org/10.5194/nhess-5-853-2005>
#'
#' Brenning, A., S. Long & P. Fieguth. 2012. Detecting rock glacier
#' flow structures using Gabor filters and IKONOS imagery.
#' Remote Sensing of Environment, 125, 227-237.
#' <http://dx.doi.org/10.1016/j.rse.2012.07.005>
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
#' @examples
#'
#' ## ------------------------------------------------------------
#' ## Classification tree example using non-spatial partitioning
#' ## ------------------------------------------------------------
#'
#' # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' library(rpart)
#' mypred_part <- function(object, newdata) predict(object, newdata)[, 2]
#' ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
#' # show the effects of overfitting
#' fit <- rpart(fo, data = ecuador, control = ctrl)
#'
#' ### Non-spatial cross-validation:
#' mypred_part <- function(object, newdata) predict(object, newdata)[, 2]
#' nsp_res <- sperrorest(
#'   data = ecuador, formula = fo,
#'   model_fun = rpart,
#'   model_args = list(control = ctrl),
#'   pred_fun = mypred_part,
#'   progress = TRUE,
#'   smp_fun = partition_cv,
#'   smp_args = list(repetition = 1:2, nfold = 3)
#' )
#' summary(nsp_res$error_rep)
#' summary(nsp_res$error_fold)
#' summary(nsp_res$represampling)
#' # plot(nsp_res$represampling, ecuador)
#'
#' ### Spatial cross-validation:
#' sp_res <- sperrorest(
#'   data = ecuador, formula = fo,
#'   model_fun = rpart,
#'   model_args = list(control = ctrl),
#'   pred_fun = mypred_part,
#'   progress = TRUE,
#'   smp_fun = partition_kmeans,
#'   smp_args = list(repetition = 1:2, nfold = 3)
#' )
#' summary(sp_res$error_rep)
#' summary(sp_res$error_fold)
#' summary(sp_res$represampling)
#' # plot(sp_res$represampling, ecuador)
#'
#' smry <- data.frame(
#'   nonspat_training = unlist(summary(nsp_res$error_rep,
#'     level = 1
#'   )$train_auroc),
#'   nonspat_test = unlist(summary(nsp_res$error_rep,
#'     level = 1
#'   )$test_auroc),
#'   spatial_training = unlist(summary(sp_res$error_rep,
#'     level = 1
#'   )$train_auroc),
#'   spatial_test = unlist(summary(sp_res$error_rep,
#'     level = 1
#'   )$test_auroc)
#' )
#' boxplot(smry,
#'   col = c("red", "red", "red", "green"),
#'   main = "Training vs. test, nonspatial vs. spatial",
#'   ylab = "Area under the ROC curve"
#' )
#' @export
sperrorest <- function(formula,
                       data,
                       coords = c("x", "y"),
                       model_fun,
                       model_args = list(),
                       pred_fun = NULL,
                       pred_args = list(),
                       smp_fun = partition_cv,
                       smp_args = list(),
                       train_fun = NULL,
                       train_param = NULL,
                       test_fun = NULL,
                       test_param = NULL,
                       err_fun = err_default,
                       imp_variables = NULL,
                       imp_permutations = 1000,
                       imp_sample_from = c("test", "train", "all"),
                       importance = !is.null(imp_variables),
                       distance = FALSE,
                       do_gc = 1,
                       progress = "all",
                       benchmark = FALSE,
                       mode_rep = c("future", "sequential", "loop"),
                       mode_fold = c("sequential", "future", "loop"),
                       verbose = 0) {

  if (verbose >= 1) {
    cat("sperrorest version", as.character(packageVersion("sperrorest")), "\n")
    cat("(c) A. Brenning, P. Schratz, and contributors\n")
    cat("Cite as Brenning (2012), doi: 10.1109/igarss.2012.6352393\n")
  }

  # set global variables for R CMD Check

  current_res <- NULL
  pooled_obs_train <- NULL
  pooled_obs_test <- NULL

  # if benchmark = TRUE, start clock
  if (benchmark) {
    start_time <- Sys.time()
  }

  # Some checks:
  if (missing(model_fun)) {
    stop("'model_fun' is a required argument")
  }
  if (any(all.vars(formula) == "...")) {
    stop("formula of the form lhs ~ ... not accepted by 'sperrorest'\n
         specify all predictor variables explicitly")
  }
  stopifnot(is.function(model_fun))
  stopifnot(is.function(smp_fun))
  if (!is.null(train_fun)) {
    stopifnot(is.function(train_fun))
  }
  if (!is.null(test_fun)) {
    stopifnot(is.function(test_fun))
  }
  stopifnot(is.function(err_fun))

  if (importance) {
    stopifnot(is.numeric(imp_permutations))

    if (!is.null(imp_variables)) {
      stopifnot(is.character(imp_variables)) # nocov
    }

    imp_sample_from <- match.arg(imp_sample_from)
  }
  stopifnot(is.character(coords))
  stopifnot(length(coords) == 2)

  mode_rep <- match.arg(mode_rep)
  mode_fold <- match.arg(mode_fold)
  if ((mode_rep == "future") & (mode_fold == "future")) {
    warning("Only parallelization at either the repetition level or the fold level\nis supported. Using mode_fold = 'sequential'.")
    mode_fold <- "sequential"
  }
  mode_dist <- mode_rep

  # Check if user is trying to bypass the normal mechanism for
  # generating training and test data sets and for passing formulas:
  if (any(names(model_args) == "formula")) {
    stop("'model_args' cannot have a 'formula' element")
  }
  if (any(names(model_args) == "data")) {
    stop("'model_args' cannot have a 'data' element")
  }
  if (any(names(pred_args) == "object")) {
    stop("'pred_args' cannot have an 'object' element:\n
         this will be generated by 'sperrorest'")
  }
  if (any(names(pred_args) == "newdata")) {
    stop("'pred_args' cannot have a 'newdata' element:\n
         this will be generated by 'sperrorest'")
  }

  # account for tibbles as input
  if (any(class(data) == "tbl")) {
    data <- as.data.frame(data)
  }

  # Name of response variable:
  response <- all.vars(formula)[1]

  if (verbose >= 1)
    cat(date(), "Creating resampling object...\n")

  smp_args$data <- data
  smp_args$coords <- coords

  resamp <- do.call(smp_fun, args = smp_args)

  if (distance) {
    if (verbose >= 1)
      cat(date(), "Adding distance information to resampling object...\n")
    resamp <- add.distance(object = resamp, data = data,
                           coords = coords, fun = mean,
                           mode = mode_dist[1])
    if (verbose >= 3) {
      cat("\n-----------------------------\nResampling object:",
          "\n-----------------------------\n")
      print(resamp)
      cat("\n-----------------------------\n")
    }
  }

  res <- lapply(resamp, unclass)
  class(res) <- "sperroresterror"

  pooled_error <- NULL

  ### Permutation-based variable importance assessment (optional):
  impo <- NULL
  if (importance) {
    # Importance of which variables:
    if (is.null(imp_variables)) {
      imp_variables <- all.vars(formula)[-1]
      # imp_variables <- strsplit(as.character(formula)[3], " + ",
      #                           fixed = TRUE)[[1]]
    }
    if (length(imp_variables) == 0) {
      importance <- FALSE
      warning("importance is TRUE, but there are no predictors,\n",
              "or no predictors have been selected; using importance = FALSE.")
    }
  }
  if (importance) {
    # Dummy data structure that will later be populated with the results:
    impo <- resamp
    # Create a template that will contain results of variable importance
    # assessment:
    imp_one_rep <- as.list(rep(NA, length(imp_variables)))

    names(imp_one_rep) <- imp_variables

    tmp <- as.list(rep(NA, imp_permutations))

    names(tmp) <- as.character(seq_len(imp_permutations))
    for (vnm in imp_variables) {
      imp_one_rep[[vnm]] <- tmp
    }
    rm(tmp)
  }

  # runreps call Sun Apr  9 13:28:31 2017 ------------------------------

  # mode = "future" Sun May 21 12:04:55 2017 -----------------------------

  if (verbose >= 1)
    cat(date(), "Running the model assessment...\n")

  if (mode_rep == "sequential") {
    my_res <- lapply(seq_along(resamp), function(x) {
      runreps(
        current_sample = resamp[[x]],
        data = data,
        formula = formula,
        response = response,
        do_gc = do_gc,
        imp_one_rep = imp_one_rep,
        pred_fun = pred_fun,
        model_args = model_args,
        model_fun = model_fun,
        imp_permutations = imp_permutations,
        imp_variables = imp_variables,
        imp_sample_from = imp_sample_from,
        importance = importance,
        current_res = current_res,
        pred_args = pred_args,
        coords = coords,
        progress = progress,
        mode_fold = mode_fold,
        pooled_obs_train = pooled_obs_train,
        train_fun = train_fun,
        train_param = train_param,
        test_fun = test_fun,
        test_param = test_param,
        pooled_obs_test = pooled_obs_test,
        err_fun = err_fun,
        i = x
      )
    }
    )

  } else if (mode_rep == "future") {

    my_res <- future.apply::future_lapply(seq_along(resamp), function(x) {
      runreps(
        current_sample = resamp[[x]],
        data = data,
        formula = formula,
        response = response,
        do_gc = do_gc,
        imp_one_rep = imp_one_rep,
        pred_fun = pred_fun,
        model_args = model_args,
        model_fun = model_fun,
        imp_permutations = imp_permutations,
        imp_variables = imp_variables,
        imp_sample_from = imp_sample_from,
        importance = importance,
        current_res = current_res,
        pred_args = pred_args,
        coords = coords,
        progress = progress,
        mode_fold = mode_fold,
        pooled_obs_train = pooled_obs_train,
        train_fun = train_fun,
        train_param = train_param,
        test_fun = test_fun,
        test_param = test_param,
        pooled_obs_test = pooled_obs_test,
        err_fun = err_fun,
        i = x
      )
    },
    future.seed = TRUE
    )

  } else if (mode_rep == "loop") {

    # for loop as a safety net for debugging purposes:

    my_res <- list()
    for (i_rep in seq_along(resamp)) {
      my_res[[i_rep]] <-
        runreps(
          current_sample = resamp[[i_rep]],
          data = data,
          formula = formula,
          response = response,
          do_gc = do_gc,
          imp_one_rep = imp_one_rep,
          pred_fun = pred_fun,
          model_args = model_args,
          model_fun = model_fun,
          imp_permutations = imp_permutations,
          imp_variables = imp_variables,
          imp_sample_from = imp_sample_from,
          importance = importance,
          current_res = current_res,
          pred_args = pred_args,
          coords = coords,
          progress = progress,
          mode_fold = mode_fold,
          pooled_obs_train = pooled_obs_train,
          train_fun = train_fun,
          train_param = train_param,
          test_fun = test_fun,
          test_param = test_param,
          pooled_obs_test = pooled_obs_test,
          err_fun = err_fun,
          i = i_rep
        )
      if (verbose >= 3) {
        cat("\n-----------------------------\nResults:",
            "\n-----------------------------\n")
        print(my_res[[i_rep]])
        cat("-----------------------------\n\n")
      }
    }
  } else stop("invalid mode_rep")

  ### format parallel outputs ----

  if (verbose >= 1)
    cat(date(), "Postprocessing...\n")

  # overwrite resamp object with possibly altered resample object from
  # runfolds
  # this applies if a custom test_fun or train_fun with a sub-resampling
  # method is used
  if (!is.null(test_fun) | !is.null(train_fun)) {
    if (verbose >= 2)
      cat(date(), " - Copy possibly altered resampling object...")
    for (i in seq_along(resamp)) {
      for (j in seq_along(resamp[[i]])) {
        # ...was [[1]], which assumes that all repetitions have equal
        # number of folds.
        resamp[[i]][[j]] <- my_res[[i]][["resampling"]][[j]][[j]]
      }
    }
  }

  ## 2021-06-21:
  ## removed NA check; NAs should be handled by
  ## summary methods...

  # check if any rep is NA in all folds and if, remove entry
  # this happens e.g. in maxent #nolint

  # if (verbose >= 2)
  #   cat(date(), " - Check NAs...\n")
  #
  # check_na <- lapply(my_res, function(x) all(is.na(x))) # nolint
  # check_na_flat <- unlist(check_na)
  #
  # if (any(check_na_flat) == TRUE) {
  #   check_na <- as.numeric(which(lapply(my_res, function(x) {
  #     all(is.na(x))
  #   }) ))
  #
  #   my_res <- my_res[-check_na]
  #
  # }

  # assign names to sublists - otherwise `transfer_parallel_output` doesn't work
  if (verbose >= 2)
    cat(date(), " - Rename sublists...\n")
  for (i in seq_along(my_res)) {
    names(my_res[[i]]) <- c(
      "error", "pooled_error", "importance",
      "non-converged-folds"
    )
  }

  # flatten list & calc sum
  if (verbose >= 2)
    cat(date(), " - Flatten lists...\n")
  not_converged_folds <- sum(
    unlist(lapply(my_res,
                  function(x) unlist(x[["non-converged-folds"]]))))

  # transfer results of lapply() to respective data objects
  if (verbose >= 2)
    cat(date(), " - Transfer outputs...\n")
  my_res_mod <- transfer_parallel_output(my_res, res, impo, pooled_error) # nolint

  pooled_error <- as.data.frame(my_res_mod$pooled_error)
  rownames(pooled_error) <- NULL
  class(pooled_error) <- "sperrorestreperror"

  if (importance) {
    impo <- my_res_mod$impo
    class(impo) <- "sperrorestimportance"
  }

  if (benchmark) {
    end_time <- Sys.time()
    my_bench <- list(
      system.info = Sys.info(), t_start = start_time,
      t_end = end_time, cpu_cores = future::nbrOfWorkers(),
      # was: parallel::detectCores(), which is the number of physically
      # available cores, but only nbrOfWorkers() can be used by this process.
      runtime_performance = end_time - start_time
    )
    class(my_bench) <- "sperrorestbenchmark"
  } else {
    my_bench <- NULL
  }

  package_version <- packageVersion("sperrorest")
  class(package_version) <- "sperrorestpackageversion"

  if (verbose >= 1)
    cat(date(), "Done.\n")

  res <- list(
    error_rep = pooled_error, error_fold = my_res_mod$res,
    represampling = resamp, importance = impo, benchmark = my_bench,
    package_version = package_version
  )
  class(res) <- "sperrorest"

  if (not_converged_folds > 0) {
    if (length(smp_args$repetition) > 1) {
      smp_args$repetition <- tail(smp_args$repetition, n = 1)
    }
    # print counter
    cat(sprintf(
      "%s folds of %s total folds (%s rep * %s folds) caused errors or returned NA (e.g., did not converge).", # nolint
      not_converged_folds, smp_args$repetition * smp_args$nfold,
      smp_args$repetition, smp_args$nfold
    ))
  }

  res
}
