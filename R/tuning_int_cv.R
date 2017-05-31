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
#' sptune_rf(fo, ecuador, accelerate = 16, nfold = 5,
#' partition.fun = "partition.kmeans", rf_fun = "randomForest")
#'
#' @export
sptune_rf <- function(formula = NULL, data = NULL, accelerate = 1,
                       nfold = NULL, partition_fun = NULL,
                       rf_fun = "rfsrc", ...) {

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
  partition_args <- list(data = data, nfold = nfold, order.cluster = FALSE)
  parti <- do.call(partition_fun, args = partition_args)
  train <- data[parti[[1]][[1]]$train, ]
  test <- data[parti[[1]][[1]]$test, ]

  # Perform a complete grid search over the following range of values:
  ntree <- c(10, 30, 50, seq(100, 2500, by = 100 * accelerate))
  default_mtry <- floor(sqrt(ncol(data)))
  n_variables <- length(attr(terms(formula), "term.labels"))
  mtrys <- unique(c(default_mtry, seq(1, n_variables, by = 1)))

  # Set up variables for loop:
  n_tree <- length(ntree)
  ntrees <- rep(ntree, length(mtrys))
  mtrys <- rep(mtrys, each = n_tree)
  auroc <- rep(NA, length(ntree))

  # Calculate AUROC for all combinations of cost and gamma values:

  for (i in 1:length(ntrees)) {
    auroc[i] <- rf_cv_err(ntree = ntrees[i], mtry = mtrys[i], train = train,
                           test = test, response = response, formula = formula,
                           rf_fun = rf_fun,
                           ...)
  }

  # Identify best AUROC, or if all are NA, use defaults and issue a warning:
  if (all(is.na(auroc))) {
    ntree <- 1
    mtry <- default_mtry
    warning("all AUROCs are NA in internal cross-validation")
  } else {
    wh <- which(auroc == max(auroc, na.rm = TRUE))[1]
    ntree <- ntrees[wh]
    mtry <- mtrys[wh]
  }

  # Output on screen:
  cat("Optimal ntree: ", ntree, ";    optimal mtry: ",
      mtry,";    best auroc: ",
      max(auroc, na.rm = T), "\n", sep = "")

  # Generate the actual fit object using optimized cost and gamma parameters:

  if (is.factor(train[[response]])) {
    args <- list(formula = formula, data = train, ntree = ntree, mtry = mtry)
    fit <- do.call(rf_fun, args)
  }

  # Keep track of optimal cost and gamma values:
  fit$my_ntree <- ntree
  fit$my_ntrees <- ntrees
  fit$my_mtry <- gamma
  fit$my_mtrys <- mtrys
  fit$my_auroc <- auroc

  return(fit)
}

