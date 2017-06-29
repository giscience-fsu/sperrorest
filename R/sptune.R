#' @title sptune_svm
#' @description Tuning of a Support Vector Machine (cost & gamma) using spatial
#' cross-validation
#' @author Patrick Schratz, Alexander Brenning
#'
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#' @importFrom purrr map2 map set_names
#' @importFrom magrittr extract2
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
#' @param tuning_parameter named list of tuning parameter containing values
#' to tune over.
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
#' If `parameter1` and `parameter2` are unspecified, tuning will be performed
#' on hyperparameters 'cost' and 'gamma'.
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
#'                   tuning_parameters = list(gamma = seq(1,5),
#'                   coef0 = seq(1,4)),
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
sptune_svm <- function(formula = NULL, data = NULL, tuning_parameters = list(),
                       train = NULL, test = NULL,
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

  if (length(tuning_parameters) == 0) {
    # tuning ranges: https://stats.stackexchange.com/a/69631/101464
    # Perform a complete grid search over the following range of values:

    # cost
    if (svm_fun == "svm") {
      tuning_parameters$cost <- c(2 ^ -5, 2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3, 2 ^ 5, 2 ^ 7, 2 ^ 9,
                                  2 ^ 11, 2 ^ 13, 2 ^ 15)
    } else {
      tuning_parameters$C <- c(2 ^ -5, 2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3, 2 ^ 5, 2 ^ 7, 2 ^ 9,
                               2 ^ 11, 2 ^ 13, 2 ^ 15)
    }
    # gamma
    tuning_parameters$gamma <- c(2 ^ -15, 2 ^ -13, 2 ^ -11, 2 ^ -9, 2 ^ -7, 2 ^ -5,
                                 2 ^ -3, 2 ^ -1, 2 ^ 1, 2 ^ 3)

    # recycle vector if desired
    if (accelerate > 1) {
      tuning_parameters$cost <- tuning_parameters$cost[seq(1, length(tuning_parameters$cost), accelerate)]
      tuning_parameters$gamma <- tuning_parameters$gamma[seq(1, length(tuning_parameters$gamma), accelerate)]
    }
  }

  names <- names(tuning_parameters)

  ### Set up variables for loop:

  # for two tuning parameters
  if (length(names) == 2) {
    all_comb <- expand.grid(tuning_parameters[[names[1]]], tuning_parameters[[names[2]]])
    tuning_parameters[[names[1]]] <- all_comb$Var1
    tuning_parameters[[names[2]]] <- all_comb$Var2
  }
  # for three tuning parameters
  else if (length(names) == 3) {
    all_comb <- expand.grid(tuning_parameters[[names[1]]],
                            tuning_parameters[[names[2]]],
                            tuning_parameters[[names[3]]])
    tuning_parameters[[names[1]]] <- all_comb$Var1
    tuning_parameters[[names[2]]] <- all_comb$Var2
    tuning_parameters[[names[3]]] <- all_comb$Var3
  }
  # for four tuning parameters
  else if (length(names) == 4) {
    all_comb <- expand.grid(tuning_parameters[[names[1]]],
                            tuning_parameters[[names[2]]],
                            tuning_parameters[[names[3]]],
                            tuning_parameters[[names[4]]])
    tuning_parameters[[names[1]]] <- all_comb$Var1
    tuning_parameters[[names[2]]] <- all_comb$Var2
    tuning_parameters[[names[3]]] <- all_comb$Var3
    tuning_parameters[[names[4]]] <- all_comb$Var4
  }
  # for five tuning parameters
  # this is the maximum when using 3 'kpar' + gamma + cost
  else if (length(names) == 5) {
    all_comb <- expand.grid(tuning_parameters[[names[1]]],
                            tuning_parameters[[names[2]]],
                            tuning_parameters[[names[3]]],
                            tuning_parameters[[names[4]]],
                            tuning_parameters[[names[5]]])
    tuning_parameters[[names[1]]] <- all_comb$Var1
    tuning_parameters[[names[2]]] <- all_comb$Var2
    tuning_parameters[[names[3]]] <- all_comb$Var3
    tuning_parameters[[names[4]]] <- all_comb$Var4
    tuning_parameters[[names[5]]] <- all_comb$Var5
  }

  tuning_parameters_bak <- tuning_parameters

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

      foreach(i = 1:length(tuning_parameters[[names[1]]]), .packages = (.packages()),
              .errorhandling = "remove", .verbose = FALSE) %dopar% {

                if (length(names) == 2) {
                  tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                  tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                } else if (length(names) == 3) {
                  tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                  tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                  tuning_parameters[[names[3]]] <- tuning_parameters_bak[[names[3]]][i]
                } else if (length(names) == 4) {
                  tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                  tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                  tuning_parameters[[names[3]]] <- tuning_parameters_bak[[names[3]]][i]
                  tuning_parameters[[names[4]]] <- tuning_parameters_bak[[names[4]]][i]
                } else if (length(names) == 5) {
                  tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                  tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                  tuning_parameters[[names[3]]] <- tuning_parameters_bak[[names[3]]][i]
                  tuning_parameters[[names[4]]] <- tuning_parameters_bak[[names[4]]][i]
                  tuning_parameters[[names[5]]] <- tuning_parameters_bak[[names[5]]][i]
                }

                if (svm_fun == "ksvm" && any(names(tuning_parameters) == "sigma")) {
                  tuning_parameters$kpar <- list()
                  tuning_parameters$kpar[["sigma"]] <- tuning_parameters[["sigma"]]
                  tuning_parameters[["sigma"]] <- NULL
                } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "degree")) {
                  tuning_parameters$kpar <- list()
                  tuning_parameters$kpar[["degree"]] <-  tuning_parameters[["degree"]]
                  tuning_parameters[["degree"]] <- NULL
                } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "scale")) {
                  tuning_parameters$kpar <- list()
                  tuning_parameters$kpar[["scale"]] <-  tuning_parameters[["scale"]]
                  tuning_parameters[["scale"]] <- NULL
                } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "order")) {
                  tuning_parameters$kpar <- list()
                  tuning_parameters$kpar[["order"]] <-  tuning_parameters[["order"]]
                  tuning_parameters[["order"]] <- NULL
                } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "length")) {
                  tuning_parameters$kpar <- list()
                  tuning_parameters$kpar[["length"]] <-  tuning_parameters[["length"]]
                  tuning_parameters[["length"]] <- NULL
                } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "lambda")) {
                  tuning_parameters$kpar <- list()
                  tuning_parameters$kpar[["lambda"]] <-  tuning_parameters[["lambda"]]
                  tuning_parameters[["lambda"]] <- NULL
                }

                tuning_parameters$train <- train
                tuning_parameters$test <- test
                tuning_parameters$type <- type
                tuning_parameters$kernel <- kernel
                tuning_parameters$response <- response
                tuning_parameters$formula <- formula
                tuning_parameters$svm_fun <- svm_fun

                out <- do.call(svm_cv_err, args = list(tuning_parameters))

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

      if (length(names) == 2) {
        message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                               " %s combinations."),
                        availableCores(), length(tuning_parameters[[names[1]]])))
        message(sprintf(paste0("Unique '%s': %s.",
                               " Unique '%s': %s."),
                        names[1],
                        length(unique(tuning_parameters[[names[1]]])),
                        names[2],
                        length(unique(tuning_parameters[[names[2]]]))))
      } else if (length(names) == 3) {
        message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                               " %s combinations."),
                        availableCores(), length(tuning_parameters[[names[1]]])))
        message(sprintf(paste0("Unique '%s': %s.",
                               " Unique '%s': %s."),
                        names[1],
                        length(unique(tuning_parameters[[names[1]]])),
                        names[2],
                        length(unique(tuning_parameters[[names[2]]])),
                        names[3],
                        length(unique(tuning_parameters[[names[3]]]))))
      } else if (length(names) == 4) {
        message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                               " %s combinations."),
                        availableCores(), length(tuning_parameters[[names[1]]])))
        message(sprintf(paste0("Unique '%s': %s.",
                               " Unique '%s': %s."),
                        names[1],
                        length(unique(tuning_parameters[[names[1]]])),
                        names[2],
                        length(unique(tuning_parameters[[names[2]]])),
                        names[3],
                        length(unique(tuning_parameters[[names[3]]])),
                        names[4],
                        length(unique(tuning_parameters[[names[4]]]))))
      } else if (length(names) == 5) {
        message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                               " %s combinations."),
                        availableCores(), length(tuning_parameters[[names[1]]])))
        message(sprintf(paste0("Unique '%s': %s.",
                               " Unique '%s': %s."),
                        names[1],
                        length(unique(tuning_parameters[[names[1]]])),
                        names[2],
                        length(unique(tuning_parameters[[names[2]]])),
                        names[3],
                        length(unique(tuning_parameters[[names[3]]])),
                        names[4],
                        length(unique(tuning_parameters[[names[4]]])),
                        names[4],
                        length(unique(tuning_parameters[[names[5]]]))))
      }
    } else {
      message(sprintf("Total tuning combinations: %s.",
                      length(tuning_parameters[[names[1]]])))
      plan(sequential)
    }

    foreach(i = 1:length(tuning_parameters[[names[1]]]), .packages = (.packages()),
            .errorhandling = "remove", .verbose = FALSE) %dopar% {

              if (length(names) == 2) {
                tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
              } else if (length(names) == 3) {
                tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                tuning_parameters[[names[3]]] <- tuning_parameters_bak[[names[3]]][i]
              } else if (length(names) == 4) {
                tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                tuning_parameters[[names[3]]] <- tuning_parameters_bak[[names[3]]][i]
                tuning_parameters[[names[4]]] <- tuning_parameters_bak[[names[4]]][i]
              } else if (length(names) == 5) {
                tuning_parameters[[names[1]]] <- tuning_parameters_bak[[names[1]]][i]
                tuning_parameters[[names[2]]] <- tuning_parameters_bak[[names[2]]][i]
                tuning_parameters[[names[3]]] <- tuning_parameters_bak[[names[3]]][i]
                tuning_parameters[[names[4]]] <- tuning_parameters_bak[[names[4]]][i]
                tuning_parameters[[names[5]]] <- tuning_parameters_bak[[names[5]]][i]
              }

              if (svm_fun == "ksvm" && any(names(tuning_parameters) == "sigma")) {
                tuning_parameters$kpar <- list()
                tuning_parameters$kpar[["sigma"]] <- tuning_parameters[["sigma"]]
                tuning_parameters[["sigma"]] <- NULL
              } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "degree")) {
                tuning_parameters$kpar <- list()
                tuning_parameters$kpar[["degree"]] <-  tuning_parameters[["degree"]]
                tuning_parameters[["degree"]] <- NULL
              } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "scale")) {
                tuning_parameters$kpar <- list()
                tuning_parameters$kpar[["scale"]] <-  tuning_parameters[["scale"]]
                tuning_parameters[["scale"]] <- NULL
              } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "order")) {
                tuning_parameters$kpar <- list()
                tuning_parameters$kpar[["order"]] <-  tuning_parameters[["order"]]
                tuning_parameters[["order"]] <- NULL
              } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "length")) {
                tuning_parameters$kpar <- list()
                tuning_parameters$kpar[["length"]] <-  tuning_parameters[["length"]]
                tuning_parameters[["length"]] <- NULL
              } else if (svm_fun == "ksvm" && any(names(tuning_parameters) == "lambda")) {
                tuning_parameters$kpar <- list()
                tuning_parameters$kpar[["lambda"]] <-  tuning_parameters[["lambda"]]
                tuning_parameters[["lambda"]] <- NULL
              }

              tuning_parameters$train <- train
              tuning_parameters$test <- test
              tuning_parameters$type <- type
              tuning_parameters$kernel <- kernel
              tuning_parameters$response <- response
              tuning_parameters$formula <- formula
              tuning_parameters$svm_fun <- svm_fun

              out <- do.call(svm_cv_err, args = list(tuning_parameters))
              return(out)
            } -> perf_measures
  }

  if (average_folds == TRUE) {
    # combine lists by folds
    runfolds_merged <- do.call(Map, c(f = list, perf_measures))
    # merge folds into one list for each parameter combination
    runfolds_merged <- map(runfolds_merged, function(x) do.call(Map, c(c, x)))
    # get mean
    perf_measures <- map(runfolds_merged, function(y)
      map(y, function(x) mean(x)))
  }

  # append 'parameter1' and 'parameter2' vectors to respective lists
  if (length(names) == 2) {
    perf_measures %>%
      map2(.y = tuning_parameters[[names[1]]],
           .f = ~ plyr::mutate(.x, param1 = .y)) %>%
      map2(.y = tuning_parameters[[names[2]]],
           .f = ~ plyr::mutate(.x, param2 = .y)) -> runfolds_merged
  } else if (length(names) == 3) {
    perf_measures %>%
      map2(.y = tuning_parameters[[names[1]]],
           .f = ~ plyr::mutate(.x, param1 = .y)) %>%
      map2(.y = tuning_parameters[[names[2]]],
           .f = ~ plyr::mutate(.x, param2 = .y)) %>%
      map2(.y = tuning_parameters[[names[3]]],
           .f = ~ plyr::mutate(.x, param3 = .y)) -> runfolds_merged
  } else if (length(names) == 4) {
    perf_measures %>%
      map2(.y = tuning_parameters[[names[1]]],
           .f = ~ plyr::mutate(.x, param1 = .y)) %>%
      map2(.y = tuning_parameters[[names[2]]],
           .f = ~ plyr::mutate(.x, param2 = .y)) %>%
      map2(.y = tuning_parameters[[names[3]]],
           .f = ~ plyr::mutate(.x, param3 = .y)) %>%
      map2(.y = tuning_parameters[[names[4]]],
           .f = ~ plyr::mutate(.x, param4 = .y)) -> runfolds_merged
  } else if (length(names) == 5) {
    perf_measures %>%
      map2(.y = tuning_parameters[[names[1]]],
           .f = ~ plyr::mutate(.x, param1 = .y)) %>%
      map2(.y = tuning_parameters[[names[2]]],
           .f = ~ plyr::mutate(.x, param2 = .y)) %>%
      map2(.y = tuning_parameters[[names[3]]],
           .f = ~ plyr::mutate(.x, param3 =  .y)) %>%
      map2(.y = tuning_parameters[[names[4]]],
           .f = ~ plyr::mutate(.x, param4 = .y)) %>%
      map2(.y = tuning_parameters[[names[5]]],
           .f = ~ plyr::mutate(.x, param5 = .y)) -> runfolds_merged
  }

  # rename to meaningful variable names
  runfolds_merged <- map(runfolds_merged, function(x)
    set_names(x, c(names(runfolds_merged[[1]][1:13]), names)))

  # check for NAs, subset cost and gamma and print message
  if (any(is.na(runfolds_merged))) {
    na_index <- which(is.na(runfolds_merged))
    if (length(names) == 2) {
      tuning_parameters[[names[1]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[2]]] <- tuning_parameters[[names[1]]][-na_index]
    } else if (length(names) == 3) {
      tuning_parameters[[names[1]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[2]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[3]]] <- tuning_parameters[[names[3]]][-na_index]
    } else if (length(names) == 4) {
      tuning_parameters[[names[1]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[2]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[3]]] <- tuning_parameters[[names[3]]][-na_index]
      tuning_parameters[[names[4]]] <- tuning_parameters[[names[4]]][-na_index]
    } else if (length(names) == 5) {
      tuning_parameters[[names[1]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[2]]] <- tuning_parameters[[names[1]]][-na_index]
      tuning_parameters[[names[3]]] <- tuning_parameters[[names[3]]][-na_index]
      tuning_parameters[[names[4]]] <- tuning_parameters[[names[4]]][-na_index]
      tuning_parameters[[names[5]]] <- tuning_parameters[[names[5]]][-na_index]
    }
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

  if (length(names) == 2) {
    best_parameter1 <- tuning_parameters[[names[1]]][list_index]
    best_parameter2 <- tuning_parameters[[names[2]]][list_index]
  } else if (length(names) == 3) {
    best_parameter1 <- tuning_parameters[[names[1]]][list_index]
    best_parameter2 <- tuning_parameters[[names[2]]][list_index]
    best_parameter3 <- tuning_parameters[[names[3]]][list_index]
  } else if (length(names) == 4) {
    best_parameter1 <- tuning_parameters[[names[1]]][list_index]
    best_parameter2 <- tuning_parameters[[names[2]]][list_index]
    best_parameter3 <- tuning_parameters[[names[3]]][list_index]
    best_parameter4 <- tuning_parameters[[names[4]]][list_index]
  } else if (length(names) == 5) {
    best_parameter1 <- tuning_parameters[[names[1]]][list_index]
    best_parameter2 <- tuning_parameters[[names[2]]][list_index]
    best_parameter3 <- tuning_parameters[[names[3]]][list_index]
    best_parameter4 <- tuning_parameters[[names[4]]][list_index]
    best_parameter5 <- tuning_parameters[[names[5]]][list_index]
  }

  # output on screen:

  if (length(names) == 2) {
    if (tune == FALSE) {
      cat(sprintf(paste0("Optimal %s: %s \nOptimal %s: %s \n",
                         "best %s: %s\n", sep = ""),
                  names[[1]], best_parameter1,
                  names[[2]], best_parameter2,
                  error_measure,
                  runfolds_merged[[list_index]][[error_measure]]))
    }
  } else if (length(names) == 3) {
    if (tune == FALSE) {
      cat(sprintf(paste0("Optimal %s: %s \nOptimal %s: %s \n",
                         "Optimal %s: %s \nBest %s: %s\n", sep = ""),
                  names[[1]], best_parameter1,
                  names[[2]], best_parameter2,
                  names[[3]], best_parameter3,
                  error_measure,
                  runfolds_merged[[list_index]][[error_measure]]))
    }
  } else if (length(names) == 4) {
    if (tune == FALSE) {
      cat(sprintf(paste0("Optimal %s: %s \nOptimal %s: %s \n",
                         "Optimal %s: %s \nOptimal %s: %s \n",
                         "Best %s: %s\n", sep = ""),
                  names[[1]], best_parameter1,
                  names[[2]], best_parameter2,
                  names[[3]], best_parameter3,
                  names[[4]], best_parameter4,
                  error_measure,
                  runfolds_merged[[list_index]][[error_measure]]))
    }
  } else if (length(names) == 5) {
    if (tune == FALSE) {
      cat(sprintf(paste0("Optimal %s: %s \nOptimal %s: %s \n",
                         "Optimal %s: %s \nOptimal %s: %s \n",
                         "Optimal %s: %s \nBest %s: %s\n", sep = ""),
                  names[[1]], best_parameter1,
                  names[[2]], best_parameter2,
                  names[[3]], best_parameter3,
                  names[[4]], best_parameter4,
                  names[[5]], best_parameter5,
                  error_measure,
                  runfolds_merged[[list_index]][[error_measure]]))
    }
  }

  ### Generate the actual fit object using optimized cost and gamma parameters:

  tuning_parameters$train <- train
  tuning_parameters$test <- test
  tuning_parameters$type <- type
  tuning_parameters$kernel <- kernel
  tuning_parameters$response <- response
  tuning_parameters$formula <- formula
  tuning_parameters$svm_fun <- svm_fun

  if (is.factor(train[[response]]) && length(levels(train[[response]])) == 2) {
    prob_model <- TRUE
    probability <- TRUE
  } else {
    prob_model <- FALSE
    probability <- FALSE
  }
  # this is needed because of the S3 methods within svm() -> otherwise the
  # default methods gets called and errors. 'formula' needs to come first in
  # the passed list
  tuning_parameters$data <- tuning_parameters$train
  if (tuning_parameters$svm_fun == "svm") {
    args1 <- list()
    args1$formula <- tuning_parameters$formula
    tuning_parameters$formula <- NULL
    fit <- try(do.call(tuning_parameters$svm_fun,
                       args = c(args1, tuning_parameters)))
  } else {
    tuning_parameters$x <- tuning_parameters$formula
    tuning_parameters$formula <- NULL
    fit <- try(do.call(tuning_parameters$svm_fun, args = tuning_parameters))
  }

  if (length(names) == 2) {
    list_out <- list(fit = fit,
                     tune = list(best_parameter1,
                                 tuning_parameters[[names[1]]],
                                 best_parameter2,
                                 tuning_parameters[[names[2]]],
                                 all_error_measures,
                                 runfolds_merged[[list_index]],
                                 runfolds_merged))
    sprintf(paste0("optimal_%s,all_%s,optimal_%s,all_%s,all_error_measures,",
                   "performances_best_run,performances_all_runs", sep = ""),
            names[1], names[1], names[2], names[2]) %>%
      str_split(",") %>%
      extract2(1) -> list_names
    list_out[[2]] <- set_names(list_out[[2]], list_names)

  } else if (length(names) == 3) {
    list_out <- list(fit = fit,
                     tune = list(best_parameter1,
                                 tuning_parameters[[names[1]]],
                                 best_parameter2,
                                 tuning_parameters[[names[2]]],
                                 best_parameter3,
                                 tuning_parameters[[names[3]]],
                                 all_error_measures,
                                 runfolds_merged[[list_index]],
                                 runfolds_merged))
    sprintf(paste0("optimal_%s,all_%s,optimal_%s,all_%s,optimal_%s,all_%s,",
                   "all_error_measures,",
                   "performances_best_run,performances_all_runs", sep = ""),
            names[1], names[1], names[2], names[2], names[3], names[3]) %>%
      str_split(",") %>%
      extract2(1) -> list_names
    list_out[[2]] <- set_names(list_out[[2]], list_names)
  } else if (length(names) == 4) {
    list_out <- list(fit = fit,
                     tune = list(best_parameter1,
                                 tuning_parameters[[names[1]]],
                                 best_parameter2,
                                 tuning_parameters[[names[2]]],
                                 best_parameter3,
                                 tuning_parameters[[names[3]]],
                                 best_parameter4,
                                 tuning_parameters[[names[4]]],
                                 all_error_measures,
                                 runfolds_merged[[list_index]],
                                 runfolds_merged))
    sprintf(paste0("optimal_%s,all_%s,optimal_%s,all_%s,optimal_%s,all_%s,",
                   "optimal_%s,all_%s,all_error_measures,",
                   "performances_best_run,performances_all_runs", sep = ""),
            names[1], names[1], names[2], names[2], names[3], names[3],
            names[4], names[4]) %>%
      str_split(",") %>%
      extract2(1) -> list_names
    list_out[[2]] <- set_names(list_out[[2]], list_names)
  } else if (length(names) == 5) {
    list_out <- list(fit = fit,
                     tune = list(best_parameter1,
                                 tuning_parameters[[names[1]]],
                                 best_parameter2,
                                 tuning_parameters[[names[2]]],
                                 best_parameter3,
                                 tuning_parameters[[names[3]]],
                                 best_parameter4,
                                 tuning_parameters[[names[4]]],
                                 best_parameter5,
                                 tuning_parameters[[names[5]]],
                                 all_error_measures,
                                 runfolds_merged[[list_index]],
                                 runfolds_merged))
    sprintf(paste0("optimal_%s,all_%s,optimal_%s,all_%s,optimal_%s,all_%s,",
                   "optimal_%s,all_%s,optimal_%s,all_%s,all_error_measures,",
                   "performances_best_run,performances_all_runs", sep = ""),
            names[1], names[1], names[2], names[2], names[3], names[3],
            names[4], names[4], names[5], names[5]) %>%
      str_split(",") %>%
      extract2(1) -> list_names
    list_out[[2]] <- set_names(list_out[[2]], list_names)
  }
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
#' @param parameter1 optional user-defined vector of hyperparameter to
#' tune over. See details.
#'
#' @param parameter2 optional user-defined vector of hyperparameter to
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
#' If `parameter1` and `parameter2` are unspecified, tuning will be performed
#' on hyperparameters 'ntrees' and 'mtry'.
#'
#' The default behaviour of [sptune_rf] tunes over all possible 'mtry' values
#' (which are of `length(predictors)`) and a selection of 'ntrees' ranging
#' between 10 and 1280. Use `accelerate` to reduce the number of 'ntrees'.
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
#' @param parameter1 optional user-defined vector of hyperparameter
#' to tune over. See details.
#'
#' @param parameter2 optional user-defined vector of hyperparameter
#' to tune over. See details.
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
                          parameter1 = NULL,
                          parameter2 = NULL, tune = FALSE,
                          train = NULL, test = NULL, ...) {

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

  if (is.null(train) && is.null(test)) {
    # partition the data
    partition_args <- list(data = data, nfold = nfold,
                           ...)
    resamp <- do.call(partition_fun, args = partition_args)[[1]]
    train <- data[resamp[[1]][[1]]$train, ]
    test <- data[resamp[[1]][[1]]$test, ]
  }

  ## feature classes L, Q, H, T, LQ, HQ, LQP, LQT, QHP, QHT, QHPT,

  if (is.null(parameter1) && !is.null(parameter2)) {
    parameter1_all <- seq(-10, 20, 2)
    # remove 0
    parameter1_all <- parameter1_all[ -which(parameter1_all %in% 0)]
    parameter2_all <- parameter2
  } else if (is.null(parameter2) && !is.null(parameter1)) {
    parameter2_all <- c("L", "Q", "H", "T", "LQ", "HQ", "LQP", "LQT", "QHP",
                        "QHT", "QHPT")
    parameter1_all <- parameter1
    if (0 %in% parameter1_all) {
      parameter1_all <- parameter1_all[ -which(parameter1_all %in% 0)]
    }
  } else {
    parameter1_all <- seq(-10, 20, 2)
    # remove 0
    parameter1_all <- parameter1_all[ -which(parameter1_all %in% 0)]
    parameter2_all <- c("L", "Q", "H", "T", "LQ", "HQ", "LQP", "LQT", "QHP",
                        "QHT", "QHPT")
  }

  # Set up variables for loop:
  parameter1 <- length(parameter1_all)
  parameter1_all <- rep(parameter1_all, length(parameter2_all))
  parameter2_all <- rep(parameter2_all, each = parameter1)

  message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                         " %s combinations."),
                  availableCores(), length(parameter1_all)))
  message(sprintf(paste0("Unique 'beta_multiplier': %s.",
                         " Unique 'feature_classes': %s."),
                  length(unique(parameter1_all)),
                  length(unique(parameter2_all))))

  registerDoFuture()

  if (average_folds == TRUE) {

    perf_measures <- list()
    for (f in 1:length(resamp)) {

      cat(sprintf("Fold %s\n", f))

      train <- x[resamp[[f]]$train, ]
      test <- x[resamp[[f]]$test, ]

      if (tune == FALSE) {
        cl <- makeCluster(availableCores())
        plan(cluster, workers = cl)
      } else {
        plan(sequential)
      }

      foreach(i = 1:length(parameter1_all), .packages = (.packages()),
              .errorhandling = "remove", .verbose = FALSE) %dopar% {

                out <- maxent_cv_err(beta_multiplier = parameter1_all[i],
                                     feature_classes = parameter2_all[i],
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
  } else {

    if (tune == FALSE) {
      cl <- makeCluster(availableCores())
      plan(cluster, workers = cl)
      message(sprintf(paste0("Using 'foreach' parallel mode with %s cores on",
                             " %s combinations."),
                      availableCores(), length(parameter1_all)))
      message(sprintf(paste0("Unique 'beta_multiplier': %s.",
                             " Unique 'feature_classes': %s."),
                      length(unique(parameter1_all)),
                      length(unique(parameter2_all))))
    } else {
      message(sprintf(paste0("Unique 'beta_multiplier': %s.",
                             " Unique 'feature_classes': %s.",
                             " Total tuning combinations: %s."),
                      length(unique(parameter1_all)),
                      length(unique(parameter2_all)),
                      length(parameter1_all)))
      plan(sequential)
    }

    foreach(i = 1:length(parameter1_all), .packages = (.packages()),
            .errorhandling = "remove", .verbose = FALSE) %dopar% {

              out <- maxent_cv_err(beta_multiplier = parameter1_all[i],
                                   feature_classes = parameter2_all[i],
                                   train = train, test = test,
                                   x = x, p = p, absence = absence)
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

    # append 'beta_multiplier' and 'feature_classes' vectors to respective lists
    runfolds_merged %>%
      map2(.y = parameter2_all,
           .f = ~ plyr::mutate(.x, feature_classes = .y)) %>%
      map2(.y = parameter1_all,
           .f = ~ plyr::mutate(.x, beta_multiplier = .y)) -> runfolds_merged
  } else {
    # append 'parameter1' and 'parameter2' vectors to respective lists
    perf_measures %>%
      map2(.y = parameter1_all,
           .f = ~ plyr::mutate(.x, parameter1 = .y)) %>%
      map2(.y = parameter2_all,
           .f = ~ plyr::mutate(.x, parameter2 = .y)) -> runfolds_merged
  }

  tmp1 <- check_response_type(p, error_measure,
                              runfolds_merged, option = TRUE)
  error_measure <- tmp1[[1]]
  list_index <- tmp1[[2]]

  runfolds_merged %>%
    map(error_measure) %>%
    unlist() -> all_error_measures

  best_beta_multiplier <- parameter1_all[list_index]
  best_feature_class <- parameter2_all[list_index]

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
                               parameter1_all,
                               best_feature_class,
                               parameter2_all,
                               all_error_measures,
                               runfolds_merged[[list_index]],
                               runfolds_merged))
  set_names(list_out[[2]], c("optimal_beta_multiplier", "all_beta_multiplier",
                             "optimal_feature_classes", "all_feature_classes",
                             "all_error_measures",
                             "performances_best_run", "performances_all_runs"))
  return(list_out)
}

