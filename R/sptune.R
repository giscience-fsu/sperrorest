#' @title sptune_svm
#' @description Tuning of a Support Vector Machine (cost & gamma) using spatial
#' cross-validation
#' @author Patrick Schratz, Alexander Brenning
#'
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#' @importFrom purrr map2 map
#' @importFrom utils tail
#'
#' @param formula formula.
#'
#' @param data dataframe
#'
#' @param accelerate option to speed up tuning using less 'cost' and
#' 'gamma' options. Default to `accelerate = 1`.
#' Increase to reduce number of tuning combinations.
#'
#' @param nfold number of folds for cross-validation.
#'
#' @param partition_fun character. Data partitioning method
#' (e.g. [partition_kmeans])
#'
#' @param svm_fun character. Which R svm package to use. See details.
#'
#' @param error_measure which error measure to use for optimization.
#' Default to 'RMSE' for numeric responses, 'AUROC' for binary classification
#' and 'error' for multiclass classiciation.
#'
#' @param cost optional user-defined vector of 'cost' hyperparameter to
#' tune over. See details.
#'
#' @param gamma optional user-defined vector of 'gamma' hyperparameter to
#' tune over. See details.
#'
#' @param kernel the kernel to use.
#'
#' @param type classification type to use. See [svm] or [ksvm] for details.
#'
#' @param ... additional options passed to `partition_fun`.
#'
#' @details This function tunes a Support Vector Machine either from `e1071`
#' or `kernlab` package using (spatial) cross-validation.
#'
#' @details
#' Tuning is performed over the following ranges ([reference](https://stats.stackexchange.com/questions/43943/which-search-range-for-determining-svm-optimal-c-and-gamma-parameters)):
#' \tabular{rr}{
#' cost: \tab 2^{-5}, 2^{-3}, ..., 2^{15}\cr
#' gamma: \tab 2^{-15}, 2^{-13}, ..., 2^{3}
#' }
#'
#' `error_measure` can be specified by the user, selecting one of the returned
#' error measures of [sptune_svm]. However, note that for
#' regression type responses always the minimum value of the passed error measure
#' is chosen and for classification cases the highest.
#'
#' `sptune_svm` is parallelized and runs on all possible cores.
#'
#' @seealso [plot_hyper_svm]
#'
#' @references Brenning, A., Long, S., & Fieguth, P. (2012).
#' Detecting rock glacier flow structures using Gabor filters and IKONOS
#' imagery. Remote Sensing of Environment, 125, 227-237.
#' doi:10.1016/j.rse.2012.07.005
#'
#' Pena, M. A., & Brenning, A. (2015). Assessing fruit-tree crop classification
#' from Landsat-8 time series for the Maipo Valley, Chile. Remote Sensing of
#' Environment, 171, 234-244. doi:10.1016/j.rse.2015.10.029
#'
#' @examples
#' \dontrun{
#' ##------------------------------------------------------------
#' ## binary classification
#' ##------------------------------------------------------------
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_svm(fo, ecuador, accelerate = 8, nfold = 5,
#'                   partition_fun = "partition_kmeans", svm_fun = "svm",
#'                   kernel = "sigmoid", type = "C-classification")
#'
#' ##------------------------------------------------------------
#' ## multiclass classification
#' ##------------------------------------------------------------
#' fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
#'       ndvi02 + ndvi03 + ndvi04
#' data(maipo)
#' out <- sptune_svm(fo, maipo, accelerate = 8, nfold = 5,
#'                   coords = c("utmx", "utmy"),
#'                   partition_fun = "partition_kmeans",
#'                   svm_fun = "ksvm", type = "C-svc", kernel = "rbfdot")
#'
#' ##------------------------------------------------------------
#' ## regression
#' ##------------------------------------------------------------
#'
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_svm(fo, ecuador, accelerate = 8, nfold = 5,
#'                   partition_fun = "partition_kmeans", svm_fun = "svm",
#'                   kernel = "radial", type = "eps-regression")
#' }
#' @export
sptune_svm <- function(formula = NULL, data = NULL, parameter1 = NULL,
                       parameter2 = NULL, train = NULL, test = NULL,
                       average_folds = FALSE,
                       accelerate = 1, nfold = NULL, partition_fun = NULL,
                       kernel = NULL, type = NULL, error_measure = NULL,
                       svm_fun = "svm", tune = FALSE, ...) {

  if (is.null(partition_fun) && tune == FALSE) {
    message("Partitioning method: 'partition_kmeans'.")
    partition_fun <- "partition_kmeans"
  } else {
    message(sprintf("Partitioning method: '%s'.", partition_fun))
  }

  if (is.null(nfold) && tune == FALSE) {
    nfold <- 5
    warning(sprintf("Using %s folds since 'nfold' was not set.", nfold))
  }

  if (is.null(kernel)) {
    stop("Please specify a kernel.")
  }

  response <- as.character(formula)[2]

  if (is.null(train) && is.null(test)) {
    # partition the data
    partition_args <- list(data = data, nfold = nfold,
                           ...)
    resamp <- do.call(partition_fun, args = partition_args)[[1]]
    train <- data[resamp[[1]]$train, ]
    test <- data[resamp[[1]]$test, ]
  }

  if (is.null(parameter1) && is.null(parameter2)) {
    # tuning ranges: https://stats.stackexchange.com/a/69631/101464
    # Perform a complete grid search over the following range of values:

    # costs_all <- 10 ^ seq(2 ^ -5, 2 ^ 15, by = 2 ^ 2 * accelerate)
    parameter1_all <- c(2 ^ -5, 2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3, 2 ^ 5, 2 ^ 7, 2 ^ 9,
                        2 ^ 11, 2 ^ 13, 2 ^ 15)
    # default_gamma <- 1 / length(strsplit(as.character(formula)[3], "+",
    #                                      fixed = TRUE)[[1]])
    # gammas_all <- unique(c(default_gamma,
    #                        10 ^ seq(-4, 1, by = 0.5 * accelerate)))
    parameter2_all <- c(2 ^ -15, 2 ^ -13, 2 ^ -11, 2 ^ -9, 2 ^ -7, 2 ^ -5,
                        2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3)

    # recycle vector if desired
    if (accelerate > 1) {
      parameter1_all <- parameter1_all[seq(1, length(parameter1_all), accelerate)]
      parameter2_all <- parameter2_all[seq(1, length(parameter2_all), accelerate)]
    }
  } else {
    parameter1_all <- parameter1
    parameter2_all <- parameter2
  }

  # Set up variables for loop:
  n_cost <- length(parameter1_all)
  parameter1_all <- rep(parameter1_all, length(parameter2_all))
  parameter2_all <- rep(parameter2_all, each = n_cost)

  #  for some reason we need to initialize 'i' here to suppress
  # 'no-visible-binding-for-global-variable' warning
  i <- NULL
  registerDoFuture()

  if (average_folds == TRUE) {
    perf_measures <- list()
    for (f in 1:length(resamp)) {

      cat(sprintf("Fold %s\n", f))

      train <- data[resamp[[f]]$train, ]
      test <- data[resamp[[f]]$test, ]

      if (tune == FALSE) {
        cl <- makeCluster(availableCores())
        plan(cluster, workers = cl)
      } else {
        plan(sequential)
      }

      foreach(i = 1:length(parameter1_all), .packages = (.packages()),
              .errorhandling = "remove", .verbose = FALSE) %dopar% {

                out <- svm_cv_err(parameter1 = parameter1_all[i],
                                  parameter2 = parameter2_all[i],
                                  train = train, test = test, type = type,
                                  kernel = kernel,
                                  response = response, formula = formula,
                                  svm_fun = svm_fun)
                return(out)
              } -> perf_measures[[f]]
      stopCluster(cl)
    }

    # combine lists by folds
    runfolds_merged <- do.call(Map, c(f = list,perf_measures))
    # merge folds into one list for each parameter combination
    runfolds_merged <- map(runfolds_merged, function(x) do.call(Map,c(c, x)))
    # get mean
    runfolds_merged <- map(runfolds_merged, function(y)
      map(y, function(x) mean(x)))
  } else {

    if (tune == FALSE) {
      cl <- makeCluster(availableCores())
      plan(cluster, workers = cl)
      message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                             " %s combinations."),
                      availableCores(), length(parameter1_all)))
      message(sprintf(paste0("Unique 'cost': %s.",
                             " Unique 'gamma': %s."),
                      length(unique(parameter1_all)),
                      length(unique(parameter2_all))))
    } else {
      message(sprintf(paste0("Unique 'cost': %s.",
                             " Unique 'gamma': %s.",
                             " Total tuning combinations: %s."),
                      length(unique(parameter1_all)),
                      length(unique(parameter2_all)),
                      length(parameter1_all)))
      plan(sequential)
    }

    foreach(i = 1:length(parameter1_all), .packages = (.packages()),
            .errorhandling = "remove", .verbose = FALSE) %dopar% {

              out <- svm_cv_err(parameter1 = parameter1_all[i],
                                parameter2 = parameter2_all[i],
                                train = train, test = test, type = type,
                                kernel = kernel,
                                response = response, formula = formula,
                                svm_fun = svm_fun)
              return(out)
            } -> perf_measures
  }

  if (average_folds == TRUE) {
    # combine lists by folds
    runfolds_merged <- do.call(Map, c(f = list, perf_measures))
    # merge folds into one list for each parameter combination
    runfolds_merged <- map(runfolds_merged, function(x) do.call(Map, c(c, x)))
    # get mean
    runfolds_merged <- map(runfolds_merged, function(y)
      map(y, function(x) mean(x)))

    # append 'parameter1' and 'parameter2' vectors to respective lists
    runfolds_merged %>%
      map2(.y = parameter1_all,
           .f = ~ plyr::mutate(.x, parameter1 = .y)) %>%
      map2(.y = parameter2_all,
           .f = ~ plyr::mutate(.x, parameter2 = .y)) -> runfolds_merged
  } else {
    # append 'parameter1' and 'parameter2' vectors to respective lists
    perf_measures %>%
      map2(.y = parameter1_all,
           .f = ~ plyr::mutate(.x, parameter1 = .y)) %>%
      map2(.y = parameter2_all,
           .f = ~ plyr::mutate(.x, parameter2 = .y)) -> runfolds_merged
  }

  # check for NAs, subset cost and gamma and print message
  if (any(is.na(runfolds_merged))) {
    na_index <- which(is.na(runfolds_merged))
    parameter1_all <- parameter1_all[-na_index]
    parameter2_all <- parameter2_all[-na_index]
    runfolds_merged <- runfolds_merged[-na_index]

    message(sprintf(paste0("Removed %s combinations due to non-convergence.\n"),
                    length(na_index)))
  }

  tmp1 <- check_response_type(train[[response]], error_measure,
                              runfolds_merged, option = TRUE)
  error_measure <- tmp1[[1]]
  list_index <- tmp1[[2]]

  runfolds_merged %>%
    map(error_measure) %>%
    unlist() -> all_error_measures

  best_cost <- parameter1_all[list_index]
  best_gamma <- parameter2_all[list_index]

  # output on screen:
  if (tune == FALSE) {
    cat(sprintf(paste0("Optimal cost: ", best_cost, ";    optimal gamma: ",
                       best_gamma,";    best %s: ",
                       runfolds_merged[[list_index]][[error_measure]],
                       "\n", sep = ""),
                error_measure))
  }

  ### Generate the actual fit object using optimized cost and gamma parameters:

  # account for binary c
  if (is.factor(train[[response]]) && length(levels(train[[response]])) == 2) {
    prob_model <- TRUE
    probability <- TRUE
  } else {
    prob_model <- FALSE
    probability <- FALSE
  }
  if (svm_fun == "ksvm") {
    args <- list(x = formula, data = train, type = type, kernel = kernel,
                 prob.model = prob_model, C = best_cost, gamma = best_gamma)
  } else if (svm_fun == "SVM") {
    args <- list(formula = formula, data = train, type = type,
                 kernel = kernel, probability = probability,
                 C = best_cost, gamma = best_gamma)
  } else if (svm_fun == "svm") {
    args <- list(formula = formula, data = train, type = type,
                 kernel = kernel, probability = probability,
                 cost = best_cost, gamma = best_gamma)
  }
  fit <- do.call(svm_fun, args)

  # create return list
  list_out <- list(fit = fit,
                   tune = list(best_cost,
                               parameter1_all,
                               best_gamma,
                               parameter2_all,
                               all_error_measures,
                               runfolds_merged[[list_index]],
                               runfolds_merged))
  set_names(list_out[[2]], c("optimal_cost", "all_costs", "optimal_gamma",
                             "all_gammas", "all_error_measures",
                             "performances_best_run", "performances_all_runs"))
  return(list_out)
}

#' @title sptune_rf
#' @description Tuning of Random Forest (mtry & ntrees) using spatial
#' cross-validation
#' @author Patrick Schratz, Alexander Brenning
#'
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#' @importFrom purrr map2 map
#' @importFrom utils tail
#'
#' @param formula formula.
#'
#' @param data dataframe.
#'
#' @param step_factor option to speed up tuning using less 'ntrees' options.
#' Default to `step_factor = 2`. Increase to reduce number of 'ntrees' for
#' tuning.
#'
#' @param nfold number of folds for cross-validation.
#'
#' @param partition_fun character. Data partitioning method
#' (e.g. [partition_kmeans])
#'
#' @param rf_fun character. Which R Random Forest package to use. See details.
#'
#' @param error_measure which error measure to use for optimization.
#' Default to 'RMSE' for numeric responses, 'AUROC' for binary classification
#' and 'error' for multiclass classiciation.
#'
#' @param mtrys optional user-defined vector of 'mtry' hyperparameter to
#' tune over. See details.
#'
#' @param ntrees optional user-defined vector of 'ntrees' hyperparameter to
#' tune over. See details.
#'
#' @param ... additional options passed to `partition_fun`.
#'
#' @param importance whether to calculate variable importance on the best
#' model selected by tuning. See `?rfsrc` or `?randomForest` for more details
#' on how to set this argument correctly.
#'
#' @details This function tunes a Random Forest model either from [randomForest],
#' or [randomForestSRC] package using (spatial) cross-validation.
#'
#' `error_measure` can be specified by the user, selecting one of the returned
#' error measures of [sptune_rf]. However, note that for
#' regression type responses always the minimum value of the passed error measure
#' is chosen and for classification cases the highest.
#'
#' The default behaviour of [sptune_rf] tunes over all possible 'mtry' values
#' (which are of `length(predictors)`) and a selection of 'ntrees' ranging
#' between 10 and 1000. Use `accelerate` to reduce the number of 'ntrees'.
#' Specify a custom vector if you want to modify the number of `mtry` used
#' for testing. This is useful if the model contains > 20
#' predictors but runtime depends on your cpu power / number of cores.
#'
#' `sptune_rf` is parallelized and runs on all possible cores.
#'
#' @seealso [plot_hyper_rf]
#'
#' @references Brenning, A., Long, S., & Fieguth, P. (2012).
#' Detecting rock glacier flow structures using Gabor filters and IKONOS
#' imagery. Remote Sensing of Environment, 125, 227-237.
#' doi:10.1016/j.rse.2012.07.005
#'
#' Pena, M. A., & Brenning, A. (2015). Assessing fruit-tree crop classification
#' from Landsat-8 time series for the Maipo Valley, Chile. Remote Sensing of
#' Environment, 171, 234-244. doi:10.1016/j.rse.2015.10.029
#'
#' @examples
#' \dontrun{
#' ##------------------------------------------------------------
#' ## binary classification
#' ##------------------------------------------------------------
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_rf(fo, ecuador, step_factor = 16, nfold = 5,
#' partition_fun = "partition_kmeans", rf_fun = "randomForest")
#'
#' ##------------------------------------------------------------
#' ## multiclass classification
#' ##------------------------------------------------------------
#' fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
#'       ndvi02 + ndvi03 + ndvi04
#' data(maipo)
#' out <- sptune_rf(fo, maipo, step_factor = 32, nfold = 5,
#'                  coords = c("utmx", "utmy"),
#'                  partition_fun = "partition_kmeans",
#'                  rf_fun = "randomForest")
#'
#' ##------------------------------------------------------------
#' ## regression
#' ##------------------------------------------------------------
#'
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_rf(fo, ecuador, step_factor = 16, nfold = 5,
#' partition_fun = "partition_kmeans", rf_fun = "randomForest")
#' }
#' @export
sptune_rf <- function(formula = NULL, data = NULL, step_factor = 2,
                      nfold = NULL, partition_fun = NULL,
                      train = NULL, test = NULL,
                      average_folds = FALSE, tune = FALSE,
                      rf_fun = "rfsrc", error_measure = NULL,
                      parameter1 = NULL, parameter2 = NULL,
                      importance = "none", ...) {

  if (is.null(partition_fun) && tune == FALSE) {
    message("Partitioning method: 'partition_kmeans'.")
    partition_fun <- "partition_kmeans"
  } else {
    message(sprintf("Partitioning method: '%s'.", partition_fun))
  }

  if (is.null(nfold) && tune == FALSE) {
    nfold <- 5
    warning(sprintf("Using %s folds since 'nfold' was not set.", nfold))
  }

  response <- as.character(formula)[2]

  if (is.null(train) && is.null(test)) {
    # partition the data
    partition_args <- list(data = data, nfold = nfold,
                           ...)
    resamp <- do.call(partition_fun, args = partition_args)[[1]]
    train <- data[resamp[[1]]$train, ]
    test <- data[resamp[[1]]$test, ]
  }

  if (is.null(parameter1) && is.null(parameter2)) {
    # make sure that step_factor is not 1; otherwise inf while loop
    if (step_factor == 1) {
      step_factor <- 2
      message(paste0("'step_factor' must be > 1; setting it to '2'."))
    }

    # Perform a complete grid search over the following range of values:
    parameter1_all <- c(10)
    while (tail(parameter1_all, n = 1) < 1000) {
      i <- tail(parameter1_all, n = 1) * step_factor
      parameter1_all <- c(parameter1_all, i)
    }
    default_parameter2 <- floor(sqrt(ncol(train)))
    n_variables <- length(attr(terms(formula), "term.labels"))
    parameter2_all <- unique(c(default_parameter2, seq(1, n_variables, by = 1)))
  } else {
    parameter1_all <- parameter2
    parameter2_all <- parameter1
  }



  # Set up variables for loop:
  n_parameter1 <- length(parameter1_all)
  parameter1_all <- rep(parameter1_all, length(parameter2_all))
  parameter2_all <- rep(parameter2_all, each = n_parameter1)

  registerDoFuture()

  if (average_folds == TRUE) {
    perf_measures <- list()
    for (f in 1:length(resamp)) {

      cat(sprintf("Fold %s\n", f))

      train <- data[resamp[[f]]$train, ]
      test <- data[resamp[[f]]$test, ]

      if (tune == FALSE) {
        cl <- makeCluster(availableCores())
        plan(cluster, workers = cl)
      } else {
        plan(sequential)
      }

      foreach(i = 1:length(parameter1_all), .packages = (.packages()),
              .errorhandling = "remove", .verbose = FALSE) %dopar% {

                out <- rf_cv_err(ntree = parameter1_all[i], mtry = parameter2_all[i],
                                 train = train, test = test,
                                 response = response, formula = formula,
                                 rf_fun = rf_fun)
                return(out)
              } -> perf_measures[[f]]
      stopCluster(cl)
    }

    # combine lists by folds
    runfolds_merged <- do.call(Map, c(f = list,perf_measures))
    # merge folds into one list for each parameter combination
    runfolds_merged <- map(runfolds_merged, function(x) do.call(Map,c(c, x)))
    # get mean
    runfolds_merged <- map(runfolds_merged, function(y)
      map(y, function(x) mean(x)))
  } else {

    if (tune == FALSE) {

      cl <- makeCluster(availableCores())
      plan(cluster, workers = cl)
      message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                             " %s combinations."),
                      availableCores(), length(parameter1_all)))
      message(sprintf(paste0("Unique 'ntrees': %s.",
                             " Unique 'mtry': %s."),
                      length(unique(parameter1_all)),
                      length(unique(parameter2_all))))
    } else {

      message(sprintf(paste0("Unique 'ntrees': %s.",
                             " Unique 'mtry': %s.",
                             " Total tuning combinations: %s."),
                      length(unique(parameter1_all)),
                      length(unique(parameter2_all)),
                      length(parameter1_all)))
      plan(sequential)
    }

    foreach(i = 1:length(parameter1_all), .packages = (.packages()),
            .errorhandling = "remove", .verbose = FALSE) %dopar% {

              out <- rf_cv_err(parameter1 = parameter1_all[i],
                               parameter2 = parameter2_all[i],
                               train = train, test = test,
                               response = response, formula = formula,
                               rf_fun = rf_fun)
              return(out)
            } -> perf_measures
  }

  if (average_folds == TRUE) {
    # combine lists by folds
    runfolds_merged <- do.call(Map, c(f = list, perf_measures))
    # merge folds into one list for each parameter combination
    runfolds_merged <- map(runfolds_merged, function(x) do.call(Map, c(c, x)))
    # get mean
    runfolds_merged <- map(runfolds_merged, function(y)
      map(y, function(x) mean(x)))

    # append 'parameter1' and 'parameter2' vectors to respective lists
    runfolds_merged %>%
      map2(.y = parameter1_all,
           .f = ~ plyr::mutate(.x, parameter1 = .y)) %>%
      map2(.y = parameter2_all,
           .f = ~ plyr::mutate(.x, parameter2 = .y)) -> runfolds_merged
  } else {
    # append 'parameter1' and 'parameter2' vectors to respective lists
    perf_measures %>%
      map2(.y = parameter1_all,
           .f = ~ plyr::mutate(.x, parameter1 = .y)) %>%
      map2(.y = parameter2_all,
           .f = ~ plyr::mutate(.x, parameter2 = .y)) -> runfolds_merged
  }

  tmp1 <- check_response_type(train[[response]], error_measure,
                              runfolds_merged, option = TRUE)
  error_measure <- tmp1[[1]]
  list_index <- tmp1[[2]]

  runfolds_merged %>%
    map(error_measure) %>%
    unlist() -> all_error_measures

  best_ntree <- parameter1_all[list_index]
  best_mtry <- parameter2_all[list_index]

  if (tune == FALSE) {
    # output on screen:
    cat(sprintf(paste0("Optimal ntree: ", best_ntree, ";    optimal mtry: ",
                       best_mtry,";    best %s: ",
                       runfolds_merged[[list_index]][[error_measure]],
                       "\n", sep = ""),
                error_measure))
  }

  # account for different default setting of importance argument
  if (rf_fun == "randomForest" && importance == "none") {
    importance <- FALSE
  }

  # Generate the actual fit object using optimized hyperparameters:
  args <- list(formula = formula, data = train, ntree = best_ntree,
               mtry = best_mtry, importance = importance)
  fit <- do.call(rf_fun, args)

  list_out <- list(fit = fit,
                   tune = list(best_ntree,
                               parameter1_all,
                               best_mtry,
                               parameter2_all,
                               all_error_measures,
                               runfolds_merged[[list_index]],
                               runfolds_merged))
  set_names(list_out[[2]], c("optimal_ntree", "all_ntrees", "optimal_mtry",
                             "all_mtrys", "all_error_measures",
                             "performances_best_run", "performances_all_runs"))
  return(list_out)
}

#' @title sptune_maxent
#' @description Tuning of Maxent (regularization parameter \eqn{\beta} &
#' feature classes) using spatial cross-validation.
#' @author Patrick Schratz, Alexander Brenning
#'
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#' @importFrom purrr map2 map
#' @importFrom utils tail
#'
#' @param x Predictors. Raster* object or SpatialGridDataFrame, containing
#' grids with predictor variables. These will be used to extract values from
#' for the point locations. x can also be a data.frame, in which case each
#' column should be a predictor variable and each row a presence or background
#' record
#'
#' @param p Occurrence data. This can be a data.frame, matrix, SpatialPoints
#' object, or a vector. If p is a data.frame or matrix it represents a set of
#' point locations; and it must have two columns with the first being the
#' x-coordinate (longitude) and the second the y-coordinate (latitude).
#' Coordinates can also be specified with a SpatialPoints* object
#'
#' @param data dataframe.
#'
#' @param beta_multiplier optional user-defined vector of 'beta_multiplier'
#' hyperparameter to tune over. See details.
#'
#' @param feature_classes optional user-defined vector of 'feature_classes'
#' hyperparameter to tune over. See details.
#'
#' @param absence is the data of type 'presence-absence'?
#'
#' @param nfold number of folds for cross-validation.
#'
#' @param partition_fun character. Data partitioning method
#' (e.g. [partition_kmeans])
#'
#' @param error_measure which error measure to use for optimization.
#' Default to 'RMSE' for numeric responses, 'AUROC' for binary classification
#' and 'error' for multiclass classiciation.
#'
#' @param ... additional options passed to `partition_fun`.
#'
#' @details This function tunes a Maxent model from the [dismo] package
#' using (spatial) cross-validation.
#'
#' `error_measure` can be specified by the user, selecting one of the returned
#' error measures of [sptune_maxent].
#'
#' `sptune_maxent` is parallelized and runs on all possible cores.
#'
#' Tuning is performed over the following ranges:
#' \tabular{rr}{
#' beta: \tab -10, -8, -6, -4, -2, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20\cr
#' feature classes: \tab "L", "Q", "H", "T", "LQ", "HQ", "LQP", "LQT", "QHP",
#' "QHT", "QHPT"
#' }
#'
#' Currently, fitting/testing for every parameter combination is only performed
#' on 1 of the partitioned folds. Hence, results reflect the performance on one
#' fold and are not averaged across folds.
#'
#' WARNING: Setting `absence = TRUE` will alter the predicted values from
#' relative probabilities (presence-only) to probabilities (presence-absence).
#' Make sure to set the switch according to your data type. Results will be
#' biased otherwise!
#'
#' @seealso [plot_hyper_maxent]
#'
#' @references Guillera-Arroita, G., Lahoz-Monfort, J. J., & Elith, J. (2014).
#' Maxent is not a presence-absence method: a comment on Thibaudet al. Methods
#' in Ecology and Evolution, 5(11), 1192-1197. doi:10.1111/2041-210x.12252
#'
#' @examples
#' \dontrun{
#' data(maxent_pred)
#' data(maxent_response)
#' data(basque)
#'
#' sptune_maxent(x = maxent_pred, p = maxent_response,
#'               data = basque, absence = TRUE)
#' }
#' @export
sptune_maxent <- function(x = NULL, p = NULL, data = NULL,
                          nfold = NULL, partition_fun = NULL,
                          error_measure = NULL, absence = FALSE,
                          beta_multiplier = NULL,
                          feature_classes = NULL, ...) {

  if (is.null(partition_fun)) {
    message("Partitioning method: 'partition_kmeans'.")
    partition_fun <- "partition_kmeans"
  } else {
    message(sprintf("Partitioning method: '%s'.", partition_fun))
  }

  if (is.null(nfold)) {
    nfold <- 5
    warning(sprintf("Using %s folds since 'nfold' was not set.", nfold))
  }

  # partition the data
  partition_args <- list(data = data, nfold = nfold,
                         ...)
  resamp <- do.call(partition_fun, args = partition_args)[[1]]

  ## feature classes L, Q, H, T, LQ, HQ, LQP, LQT, QHP, QHT, QHPT,

  if (is.null(beta_multiplier) && !is.null(feature_classes)) {
    beta_multiplier_all <- seq(-10, 20, 2)
    # remove 0
    beta_multiplier_all <- beta_multiplier_all[ -which(beta_multiplier_all %in% 0)]
    feature_classes_all <- feature_classes
  } else if (is.null(feature_classes) && !is.null(beta_multiplier)) {
    feature_classes_all <- c("L", "Q", "H", "T", "LQ", "HQ", "LQP", "LQT", "QHP",
                             "QHT", "QHPT")
    beta_multiplier_all <- beta_multiplier
  } else {
    beta_multiplier_all <- seq(-10, 20, 2)
    # remove 0
    beta_multiplier_all <- beta_multiplier_all[ -which(beta_multiplier_all %in% 0)]
    feature_classes_all <- c("L", "Q", "H", "T", "LQ", "HQ", "LQP", "LQT", "QHP",
                             "QHT", "QHPT")
  }

  # Set up variables for loop:
  beta_multiplier <- length(beta_multiplier_all)
  beta_multiplier_all <- rep(beta_multiplier_all, length(feature_classes_all))
  feature_classes_all <- rep(feature_classes_all, each = beta_multiplier)

  message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                         " %s combinations."),
                  availableCores(), length(beta_multiplier_all)))
  message(sprintf(paste0("Unique 'beta_multiplier': %s.",
                         " Unique 'feature_classes': %s."),
                  length(unique(beta_multiplier_all)),
                  length(unique(feature_classes_all))))

  perf_measures <- list()
  for (f in 1:length(resamp)) {

    cat(sprintf("Fold %s\n", f))

    train <- x[resamp[[f]]$train, ]
    test <- x[resamp[[f]]$test, ]

    registerDoFuture()
    cl <- makeCluster(availableCores())
    plan(cluster, workers = cl)

    foreach(i = 1:length(beta_multiplier_all), .packages = (.packages()),
            .errorhandling = "remove", .verbose = FALSE) %dopar% {

              out <- maxent_cv_err(beta_multiplier = beta_multiplier_all[i],
                                   feature_classes = feature_classes_all[i],
                                   train = train, test = test,
                                   x = x, p = p, absence = absence)
              return(out)
            } -> perf_measures[[f]]
    stopCluster(cl)
  }

  # combine lists by folds
  runfolds_merged <- do.call(Map, c(f = list,perf_measures))
  # merge folds into one list for each parameter combination
  runfolds_merged <- map(runfolds_merged, function(x) do.call(Map,c(c, x)))
  # get mean
  runfolds_merged <- map(runfolds_merged, function(y)
    map(y, function(x) mean(x)))

  # append 'beta_multiplier' and 'feature_classes' vectors to respective lists
  runfolds_merged %>%
    map2(.y = feature_classes_all,
         .f = ~ plyr::mutate(.x, feature_classes = .y)) %>%
    map2(.y = beta_multiplier_all,
         .f = ~ plyr::mutate(.x, beta_multiplier = .y)) -> runfolds_merged

  tmp1 <- check_response_type(p, error_measure,
                              runfolds_merged, option = TRUE)
  error_measure <- tmp1[[1]]
  list_index <- tmp1[[2]]

  runfolds_merged %>%
    map(error_measure) %>%
    unlist() -> all_error_measures

  best_beta_multiplier <- beta_multiplier_all[list_index]
  best_feature_class <- feature_classes_all[list_index]

  # output on screen:
  cat(sprintf(paste0("Optimal beta multiplier: ", best_beta_multiplier, ";    optimal feature class combination: ",
                     best_feature_class,";    best %s: ",
                     runfolds_merged[[list_index]][[error_measure]],
                     "\n", sep = ""),
              error_measure))

  # Generate the actual fit object using optimized hyperparameters:

  # account for feature classes
  if (best_feature_class == "L") {
    linear <- TRUE
    quadratic <- FALSE
    product <- FALSE
    threshold <- FALSE
    hinge <- FALSE
  } else if (best_feature_class == "Q") {
    linear <- FALSE
    quadratic <- TRUE
    product <- FALSE
    threshold <- FALSE
    hinge <- FALSE
  } else if (best_feature_class == "H") {
    linear <- FALSE
    quadratic <- FALSE
    product <- FALSE
    threshold <- FALSE
    hinge <- TRUE
  } else if (best_feature_class == "T") {
    linear <- FALSE
    quadratic <- FALSE
    product <- FALSE
    threshold <- TRUE
    hinge <- FALSE
  } else if (best_feature_class == "LQ") {
    linear <- TRUE
    quadratic <- TRUE
    product <- FALSE
    threshold <- FALSE
    hinge <- FALSE
  } else if (best_feature_class == "HQ") {
    linear <- FALSE
    quadratic <- TRUE
    product <- FALSE
    threshold <- FALSE
    hinge <- TRUE
  } else if (best_feature_class == "LQP") {
    linear <- TRUE
    quadratic <- TRUE
    product <- TRUE
    threshold <- FALSE
    hinge <- FALSE
  } else if (best_feature_class == "LQT") {
    linear <- TRUE
    quadratic <- TRUE
    product <- FALSE
    threshold <- TRUE
    hinge <- FALSE
  } else if (best_feature_class == "QHP") {
    linear <- FALSE
    quadratic <- TRUE
    product <- TRUE
    threshold <- FALSE
    hinge <- TRUE
  } else if (best_feature_class == "QHT") {
    linear <- FALSE
    quadratic <- TRUE
    product <- FALSE
    threshold <- TRUE
    hinge <- TRUE
  } else if (best_feature_class == "QHPT") {
    linear <- FALSE
    quadratic <- TRUE
    product <- TRUE
    threshold <- TRUE
    hinge <- TRUE
  }

  sprintf(paste0("betamultiplier=%s,",
                 "linear=%s,quadratic=%s,product=%s,threshold=%s,",
                 "hinge=%s"), best_beta_multiplier, linear, quadratic, product,
          threshold, hinge) -> my_args
  str_split(my_args, pattern = ",")[[1]] -> my_args

  args <- list(x = x, p = p, args = my_args)

  fit <- suppressMessages(do.call(maxent, args = args))

  list_out <- list(fit = fit,
                   tune = list(best_beta_multiplier,
                               beta_multiplier_all,
                               best_feature_class,
                               feature_classes_all,
                               all_error_measures,
                               runfolds_merged[[list_index]],
                               runfolds_merged))
  set_names(list_out[[2]], c("optimal_beta_multiplier", "all_beta_multiplier",
                             "optimal_feature_classes", "all_feature_classes",
                             "all_error_measures",
                             "performances_best_run", "performances_all_runs"))
  return(list_out)
}

