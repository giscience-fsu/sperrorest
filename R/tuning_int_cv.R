#' @title svm_tuning
#' @description Tuning of SVM (cost & gamma) using spatial cross-validation
#' @author Alexander Brenning, Patrick Schratz
#'
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#'
#' @param formula formula
#'
#' @param data data frame
#'
#' @param accelerate option to speed up tuning using less cost and gamma values.
#' Use `accelerate = 2` or `accelerate = 4` for test runs, but `accelerate = 1`
#' for actual analysis.
#'
#' @param nfold number of folds for cross-validation
#'
#' @param partition_fun method for partitioning the data
#' (e.g. [partition.kmeans])
#'
#' @param svm_fun which R svm package to use. See details.
#'
#' @param type classification type of svm classifier
#'
#' @param kernel kernel type of svm classifier
#'
#' @param ... additional options passed to [SVM]
#'
#' @details This function tunes a support vectort machine from the [e1071],
#' [gmum.r], [kernlab] packages using (spatial) cross-validation.
#'
#' Currently this function is hard-coded to a binary response variable and AUROC
#' as error measure.
#'
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' sptune_svm(fo, ecuador, accelerate = 16, nfold = 5,
#' partition.fun = "partition.kmeans", kernel = "radial",
#' type = "C-classification")
#'
#' @export
sptune_svm <- function(formula = NULL, data = NULL, accelerate = 1,
                       nfold = NULL, partition_fun = NULL,
                       kernel = NULL, type = NULL, svm_fun = "svm", ...) {


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
  partition_args <- list(data = data, nfold = nfold, order.cluster = FALSE)
  parti <- do.call(partition_fun, args = partition_args)
  train <- data[parti[[1]][[1]]$train, ]
  test <- data[parti[[1]][[1]]$test, ]

  # Perform a complete grid search over the following range of values:
  costs <- 10^seq(-2, 4, by = 0.5 * accelerate)
  default_gamma <- 1 / length(strsplit(as.character(formula)[3], "+",
                                       fixed = TRUE)[[1]])
  gammas <- unique(c(default_gamma, 10^seq(-4, 1, by = 0.5 * accelerate)))

  # Set up variables for loop:
  n.costs <- length(costs)
  costs <- rep(costs, length(gammas))
  gammas <- rep(gammas, each = n.costs)
  auroc <- rep(NA, length(costs))

  # Calculate AUROC for all combinations of cost and gamma values:

  for (i in 1:length(costs)) {
    auroc[i] <- svm_cv_err(cost = costs[i], gamma = gammas[i], train = train,
                           test = test, response = response, formula = formula,
                           kernel = kernel, type = type, svm_fun = svm_fun,
                           ...)
  }

  # Identify best AUROC, or if all are NA, use defaults and issue a warning:
  if (all(is.na(auroc))) {
    cost <- 1
    gamma <- default_gamma
    warning("all AUROCs are NA in internal cross-validation")
  } else {
    wh <- which(auroc == max(auroc, na.rm = TRUE))[1]
    cost <- costs[wh]
    gamma <- gammas[wh]
  }

  # Output on screen:
  cat("Optimal cost: ", cost, ";    optimal gamma: ",
      gamma,";    best auroc: ",
      max(auroc, na.rm = T), "\n", sep = "")

  # Generate the actual fit object using optimized cost and gamma parameters:

  if (is.factor(train[[response]])) {
    if (svm_fun == "ksvm" | svm_fun == "SVM") {
      if (svm_fun == "ksvm") {
        args <- list(x = formula, data = train, type = type, kernel = kernel,
                     prob.model = TRUE, C = cost, gamma = gamma)
      } else {
        args <- list(formula = formula, data = train, type = type, kernel = kernel,
                     probability = TRUE, C = cost, gamma = gamma)
      }
    }
    if (svm_fun == "svm") {
      args <- list(formula = formula, data = train, type = type, kernel = kernel,
                   probability = TRUE, cost = cost, gamma = gamma)
    }
    fit <- do.call(svm_fun, args)
  }

  # Keep track of optimal cost and gamma values:
  if (svm_fun == "ksvm") {
    fit@param$C <- cost
    fit@param$C_all <- costs
    fit@param$gamma <- gamma
    fit@param$gamma_all <- gammas
    fit@param$auroc_all <- auroc
  } else {
    fit$my_cost <- cost
    fit$my_costs <- costs
    fit$my_gamma <- gamma
    fit$my_gammas <- gammas
    fit$my_auroc <- auroc
  }

  return(fit)
}

#' @title sptune_rf
#' @description Tuning of Random Forest (mtry & ntrees) using spatial cross-validation
#' @author Alexander Brenning, Patrick Schratz
#'
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#' @importFrom plyr mutate
#' @importFrom purrr map2 map
#'
#' @param formula formula.
#'
#' @param data [data.frame].
#'
#' @param accelerate option to speed up tuning using less 'ntree' options.
#' Default to `accelerate = 1`. Increase to reduce number of 'ntrees' for
#' tuning.
#'
#' @param nfold number of folds for cross-validation.
#'
#' @param partition_fun method for partitioning the data
#' (e.g. [partition_kmeans])
#'
#' @param rf_fun which R Random Forest package to use. See details.
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
#' for testing. This is usually useful if the model contains more than 20
#' predictors.
#'
#' FYI: `sptune_rf` is parallelized and runs on all possible cores.
#'
#' @examples
#'
#' ##------------------------------------------------------------
#' ## binary classification
#' ##------------------------------------------------------------
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_rf(fo, ecuador, accelerate = 16, nfold = 5,
#' partition_fun = "partition_kmeans", rf_fun = "randomForest")
#' ##------------------------------------------------------------
#' ## multiclass classification
#' ##------------------------------------------------------------
#' fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
#'       ndvi02 + ndvi03 + ndvi04
#' data(maipo)
#' out <- sptune_rf(fo, maipo, accelerate = 32, nfold = 5,
#'                  coords = c("utmx", "utmy"),
#'                  partition_fun = "partition_kmeans",
#'                  rf_fun = "randomForest")
#' ##------------------------------------------------------------
#' ## regression
#' ##------------------------------------------------------------
#'
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_rf(fo, ecuador, accelerate = 16, nfold = 5,
#' partition_fun = "partition_kmeans", rf_fun = "randomForest")
#'
#' @export
sptune_rf <- function(formula = NULL, data = NULL, accelerate = 1,
                      nfold = NULL, partition_fun = NULL,
                      rf_fun = "rfsrc", error_measure = NULL,
                      mtrys = NULL, ntrees = NULL, ...) {

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
  partition_args <- list(data = data, nfold = nfold, order.cluster = FALSE,
                         ...)
  parti <- do.call(partition_fun, args = partition_args)
  train <- data[parti[[1]][[1]]$train, ]
  test <- data[parti[[1]][[1]]$test, ]

  if (is.null(mtrys) && is.null(ntrees)) {
    # Perform a complete grid search over the following range of values:
    ntrees_all <- c(10, 30, 50, seq(100, 2500, by = 100 * accelerate))
    default_mtry <- floor(sqrt(ncol(data)))
    n_variables <- length(attr(terms(formula), "term.labels"))
    mtrys_all <- unique(c(default_mtry, seq(1, n_variables, by = 1)))
  } else {
    ntrees_all <- ntrees
    mtrys_all <- mtrys
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
         .f = ~ mutate(.x, mtry = .y)) %>%
    map2(.y = ntrees_all,
         .f = ~ mutate(.x, ntree = .y)) -> perf_measures

  # binary classifcation
  if (is.factor(train[[response]]) &&
      length(levels(train[[response]])) == 2) {

    # account for error measure
    if (is.null(error_measure)) {
      error_measure <- "auroc"
    } else {
      error_measure <- error_measure
    }
    # get list index with highest auroc
    perf_measures %>%
      map(error_measure) %>%
      which.max() -> list_index
  }
  # multiclass classification
  else if (is.factor(train[[response]]) &&
           length(levels(train[[response]])) > 2) {
    if (is.null(error_measure)) {
      error_measure <- "error"
    } else {
      error_measure <- error_measure
    }
    # get list index with highest auroc
    perf_measures %>%
      map(error_measure) %>%
      which.min() -> list_index
  }
  # regression
  else if (is.numeric(train[[response]])) {
    # account for error measure
    if (is.null(error_measure)) {
      error_measure <- "rmse"
    } else {
      error_measure <- error_measure
    }
    # get list index with highest auroc
    perf_measures %>%
      map(error_measure) %>%
      which.min() -> list_index
  }

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

  # Generate the actual fit object using optimized hyperparameters:
  args <- list(formula = formula, data = train, ntree = best_ntree,
               mtry = best_mtry)
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

