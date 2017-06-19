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
sptune_svm <- function(formula = NULL, data = NULL, cost = NULL, gamma = NULL,
                       accelerate = 1, nfold = NULL, partition_fun = NULL,
                       kernel = NULL, type = NULL, error_measure = NULL,
                       svm_fun = "svm", ...) {

  if (is.null(partition_fun)) {
    message("Partitioning method: 'partition.kmeans'.")
    partition_fun <- "partition.kmeans"
  } else {
    message(sprintf("Partitioning method: '%s'.", partition_fun))
  }

  if (is.null(nfold)) {
    nfold <- 5
    warning(sprintf("Using %s folds since 'nfold' was not set.", nfold))
  }

  if (is.null(kernel)) {
    stop("Please specify a kernel.")
  }

  response <- as.character(formula)[2]

  # partition the data
  partition_args <- list(data = data, nfold = nfold,
                         ...)
  parti <- do.call(partition_fun, args = partition_args)
  train <- data[parti[[1]][[1]]$train, ]
  test <- data[parti[[1]][[1]]$test, ]

  if (is.null(cost) && is.null(gamma)) {
    # tuning ranges: https://stats.stackexchange.com/a/69631/101464
    # Perform a complete grid search over the following range of values:

    # costs_all <- 10 ^ seq(2 ^ -5, 2 ^ 15, by = 2 ^ 2 * accelerate)
    costs_all <- c(2 ^ -5, 2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3, 2 ^ 5, 2 ^ 7, 2 ^ 9,
                   2 ^ 11, 2 ^ 13, 2 ^ 15)
    # default_gamma <- 1 / length(strsplit(as.character(formula)[3], "+",
    #                                      fixed = TRUE)[[1]])
    # gammas_all <- unique(c(default_gamma,
    #                        10 ^ seq(-4, 1, by = 0.5 * accelerate)))
    gammas_all <- c(2 ^ -15, 2 ^ -13, 2 ^ -11, 2 ^ -9, 2 ^ -7, 2 ^ -5,
                    2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3)

    # recycle vector if desired
    if (accelerate > 1) {
      costs_all <- costs_all[seq(1, length(costs_all), accelerate)]
      gammas_all <- gammas_all[seq(1, length(gammas_all), accelerate)]
    }
  } else {
    costs_all <- cost
    gammas_all <- gamma
  }

  # Set up variables for loop:
  n_cost <- length(costs_all)
  costs_all <- rep(costs_all, length(gammas_all))
  gammas_all <- rep(gammas_all, each = n_cost)

  # Calculate perf. measures for all combinations of 'cost' and 'gamma'

  registerDoFuture()
  cl <- makeCluster(availableCores())
  plan(cluster, workers = cl)

  message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                         " %s combinations."),
                  availableCores(), length(costs_all)))
  message(sprintf(paste0("Unique 'cost': %s.",
                         " Unique 'gamma': %s."),
                  length(unique(costs_all)),
                  length(unique(gammas_all))))

  #  for some reason we need to initialize 'i' here to suppress
  # 'no-visible-binding-for-global-variable' warning
  i <- NULL

  foreach(i = 1:length(costs_all), .packages = (.packages()),
          .errorhandling = "remove", .verbose = FALSE) %dopar% {

            out <- svm_cv_err(cost = costs_all[i], gamma = gammas_all[i],
                              train = train, test = test, type = type,
                              kernel = kernel,
                              response = response, formula = formula,
                              svm_fun = svm_fun)
            return(out)
          } -> perf_measures
  stopCluster(cl)

  # check for NAs, subset cost and gamma and print message
  if (any(is.na(perf_measures))) {
    na_index <- which(is.na(perf_measures))
    costs_all <- costs_all[-na_index]
    gammas_all <- gammas_all[-na_index]
    perf_measures <- perf_measures[-na_index]

    message(sprintf(paste0("Removed %s combinations due to non-convergence.\n"),
                    length(na_index)))
  }

  # append 'mtrys' and 'ntrees' vectors to respective lists
  perf_measures %>%
    map2(.y = costs_all,
         .f = ~ plyr::mutate(.x, cost = .y)) %>%
    map2(.y = gammas_all,
         .f = ~ plyr::mutate(.x, gamma = .y)) -> perf_measures

  tmp1 <- check_response_type(train[[response]], error_measure,
                              perf_measures, option = TRUE)
  error_measure <- tmp1[[1]]
  list_index <- tmp1[[2]]

  perf_measures %>%
    map(error_measure) %>%
    unlist() -> all_error_measures

  best_cost <- costs_all[list_index]
  best_gamma <- gammas_all[list_index]

  # output on screen:
  cat(sprintf(paste0("Optimal cost: ", best_cost, ";    optimal gamma: ",
                     best_gamma,";    best %s: ",
                     perf_measures[[list_index]][[error_measure]],
                     "\n", sep = ""),
              error_measure))

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
                               costs_all,
                               best_gamma,
                               gammas_all,
                               all_error_measures,
                               perf_measures[[list_index]],
                               perf_measures))
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
#' between 10 and 2500. Use `accelerate` to reduce the number of 'ntrees'.
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
                      rf_fun = "rfsrc", error_measure = NULL,
                      mtrys = NULL, ntrees = NULL,
                      importance = "none", ...) {

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

  response <- as.character(formula)[2]

  # partition the data
  partition_args <- list(data = data, nfold = nfold,
                         ...)
  parti <- do.call(partition_fun, args = partition_args)
  train <- data[parti[[1]][[1]]$train, ]
  test <- data[parti[[1]][[1]]$test, ]

  if (is.null(mtrys) && is.null(ntrees)) {
    # make sure that step_factor is not 1; otherwise inf while loop
    if (step_factor == 1) {
      step_factor <- 2
      message(paste0("'step_factor' must be > 1; setting it to '2'."))
    }
    # Perform a complete grid search over the following range of values:
    ntrees_all <- c(10)
    while (tail(ntrees_all, n = 1) < 2500) {
      i <- tail(ntrees_all, n = 1) * step_factor
      ntrees_all <- c(ntrees_all, i)
    }
    default_mtry <- floor(sqrt(ncol(data)))
    n_variables <- length(attr(terms(formula), "term.labels"))
    mtrys_all <- unique(c(default_mtry, seq(1, n_variables, by = 1)))
  } else {
    ntrees_all <- ntrees
    mtrys_all <- mtrys
  }

  ntrees_all <- c(10)
  while (tail(ntrees_all, n = 1) < 2500) {
    i <- tail(ntrees_all, n = 1) * step_factor
    ntrees_all <- c(ntrees_all, i)
  }

  # Set up variables for loop:
  n_tree <- length(ntrees_all)
  ntrees_all <- rep(ntrees_all, length(mtrys_all))
  mtrys_all <- rep(mtrys_all, each = n_tree)

  # Calculate perf. measures for all combinations of 'mtry' and 'ntrees'
  registerDoFuture()
  cl <- makeCluster(availableCores())
  plan(cluster, workers = cl)

  message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                         " %s combinations."),
                  availableCores(), length(ntrees_all)))
  message(sprintf(paste0("Unique 'ntrees': %s.",
                         " Unique 'mtry': %s."),
                  length(unique(ntrees_all)),
                  length(unique(mtrys_all))))

  foreach(i = 1:length(ntrees_all), .packages = (.packages()),
          .errorhandling = "remove", .verbose = FALSE) %dopar% {

            out <- rf_cv_err(ntree = ntrees_all[i], mtry = mtrys_all[i],
                             train = train, test = test,
                             response = response, formula = formula,
                             rf_fun = rf_fun)
            return(out)
          } -> perf_measures
  stopCluster(cl)

  # append 'mtrys' and 'ntrees' vectors to respective lists
  perf_measures %>%
    map2(.y = mtrys_all,
         .f = ~ plyr::mutate(.x, mtry = .y)) %>%
    map2(.y = ntrees_all,
         .f = ~ plyr::mutate(.x, ntree = .y)) -> perf_measures

  tmp1 <- check_response_type(train[[response]], error_measure,
                              perf_measures, option = TRUE)
  error_measure <- tmp1[[1]]
  list_index <- tmp1[[2]]

  perf_measures %>%
    map(error_measure) %>%
    unlist() -> all_error_measures

  best_ntree <- ntrees_all[list_index]
  best_mtry <- mtrys_all[list_index]

  # output on screen:
  cat(sprintf(paste0("Optimal ntree: ", best_ntree, ";    optimal mtry: ",
                     best_mtry,";    best %s: ",
                     perf_measures[[list_index]][[error_measure]],
                     "\n", sep = ""),
              error_measure))

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
                               ntrees_all,
                               best_mtry,
                               mtrys_all,
                               all_error_measures,
                               perf_measures[[list_index]],
                               perf_measures))
  set_names(list_out[[2]], c("optimal_ntree", "all_ntrees", "optimal_mtry",
                             "all_mtrys", "all_error_measures",
                             "performances_best_run", "performances_all_runs"))
  return(list_out)
}

