#' @title svm_cv_err
#' @description Calculates AUROC for different gamma and cost values
#'
#' @importFrom ROCR prediction performance
#' @importFrom e1071 svm
#' @importFrom stats predict
#' @importFrom kernlab ksvm
#'
#' @param formula model formula
#'
#' @param cost cost value
#'
#' @param gamma gamma value
#'
#' @param train training data
#'
#' @param test testing data
#'
#' @param kernel to use for fitting. See [svm]
#'
#' @param type classification type to use. See [svm]
#'
#' @param response response variable
#'
#' @keywords internal
#'
#' @examples
#' parti <- partition_kmeans(ecuador, nfold = 5, order.clusters = FALSE)
#' train <- ecuador[parti[[1]][[1]]$train, ]
#' test <- ecuador[parti[[1]][[1]]$test, ]
#'
#' ##------------------------------------------------------------
#' ## binary classification
#' ##------------------------------------------------------------
#' response <- "slides"
#'
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#'                   formula = fo, kernel = "radial", type = "C-classification",
#'                   response = "slides", svm_fun = "svm")
#'
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#'                   formula = fo, kernel = "rbfdot", type = "C-svc",
#'                   response = "slides", svm_fun = "ksvm")
#'
#' ##------------------------------------------------------------
#' ## regression
#' ##------------------------------------------------------------
#'
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope
#' response <- "dem"
#' # 'svm'
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#'                   formula = fo, response = "dem", kernel = "radial",
#'                   svm_fun = "svm", type = "eps-regression")
#' # 'ksvm'
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#'                   formula = fo, response = "dem", kernel = "rbfdot",
#'                   svm_fun = "ksvm", type = "eps-svr")
#'
#' ##------------------------------------------------------------
#' ## multiclass classification
#' ##------------------------------------------------------------
#' parti <- partition_kmeans(maipo, nfold = 5, order.clusters = FALSE,
#'                           coords = c("utmx", "utmy"))
#' train <- maipo[parti[[1]][[1]]$train, ]
#' test <- maipo[parti[[1]][[1]]$test, ]
#'
#' fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
#'   b24 +
#'   b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
#'   b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
#'   b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
#'   b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
#'   ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
#'   ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
#'   ndwi08
#' data(maipo)
#' # 'svm'
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#'                   formula = fo, response = "croptype", kernel = "radial",
#'                   svm_fun = "svm", type = "C-classification")
#' # 'ksvm'
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#'                   formula = fo, response = "croptype", kernel = "rbfdot",
#'                   svm_fun = "ksvm", type = "C-svc")
#'
#' @export
svm_cv_err <- function(cost = NULL, gamma = NULL, train = NULL, test = NULL,
                       response = NULL, formula = NULL, kernel = NULL,
                       type = NULL, svm_fun = NULL, ...) {

  ### fit model
  # binary classifcation
  if (is.factor(train[[response]]) && length(levels(train[[response]])) == 2) {
    probability <- TRUE
    prob.model <- TRUE
  } else {
    probability <- FALSE
    prob.model <- FALSE
  }

  if (svm_fun == "ksvm") {
    args <- list(x = formula, data = train, type = type, kernel = kernel,
                 prob.model = prob.model, C = cost, gamma = gamma)
  } else if (svm_fun == "SVM") {
    args <- list(formula = formula, data = train, type = type,
                 kernel = kernel, probability = probability, C = cost,
                 gamma = gamma)
  } else if (svm_fun == "svm") {
    args <- list(formula = formula, data = train, type = type,
                 kernel = kernel, probability = probability, cost = cost,
                 gamma = gamma)
  }
  fit <- try(do.call(svm_fun, args))
  cat(class(fit))

  ### predict
  if (svm_fun == "ksvm") {
    if (is.factor(train[[response]]) && length(levels(train[[response]])) == 2) {
      type <- "probabilities"
    } else {
      type <- "response"
    }
    # we do not see the error message, so returning none
    pred <- tryCatch(kernlab::predict(fit, newdata = test, type = type),
                     error = function(cond) {
                       return(NA)
                     })
  } else {
    pred <- tryCatch(predict(fit, newdata = test, probability = probability),
                     error = function(cond) {
                       return(NA)
                     })
  }
  # if NA is assigned to 'pred' due to tryCatch, we break the function and
  # return NA
  if (is.na(pred)) {
    return(pred)
  }

  ### error measures
  if (svm_fun == "svm") {
    # binary classification
    if (is.factor(train[[response]]) && length(levels(train[[response]])) == 2) {
      pred <-  attr(pred, "probabilities")[, 2]
    }
    perf_measures <- err_default(test[, response], pred)
  } else if (svm_fun == "ksvm") {
    if (is.factor(train[[response]]) &&
        length(levels(train[[response]])) == 2) {
      perf_measures <- err_default(test[, response], pred[, 2])
    } else  {
      perf_measures <- err_default(test[, response], pred)
    }
  }
  return(perf_measures)
}

#' @title rf_cv_err
#' @description Calculates AUROC for different gamma and cost values
#'
#' @importFrom ROCR prediction performance
#' @importFrom randomForest randomForest
#' @importFrom randomForestSRC rfsrc
#'
#' @param formula model formula
#'
#' @param ntree number of trees to grow
#'
#' @param mtry Number of variables randomly selected as candidates for each node split.
#'
#' @param train training data
#'
#' @param test testing data
#'
#' @param response response variable
#'
#' @keywords internal
#'
#' @examples
#'
#' ##------------------------------------------------------------
#' ## binary classification
#' ##------------------------------------------------------------
#'
#' parti <- partition_kmeans(ecuador, nfold = 5, order.clusters = FALSE)
#' train <- ecuador[parti[[1]][[1]]$train, ]
#' test <- ecuador[parti[[1]][[1]]$test, ]
#' response <- "slides"
#'
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' out <- rf_cv_err(mtry = 3, ntree = 1000, train = train, test = test,
#' formula = fo, response = "slides", rf_fun = "rfsrc")
#'
#' ##------------------------------------------------------------
#' ## regression
#' ##------------------------------------------------------------
#'
#' #' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope
#' response <- "dem"
#'
#' out <- rf_cv_err(mtry = 3, ntree = 1000, train = train, test = test,
#' formula = fo, response = "dem", rf_fun = "rfsrc")
#'
#' ##------------------------------------------------------------
#' ## multiclass classification
#' ##------------------------------------------------------------
#' parti <- partition_kmeans(maipo, nfold = 5, order.clusters = FALSE,
#'                           coords = c("utmx", "utmy"))
#' train <- maipo[parti[[1]][[1]]$train, ]
#' test <- maipo[parti[[1]][[1]]$test, ]
#'
#' fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
#'   b24 +
#'   b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
#'   b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
#'   b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
#'   b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
#'   ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
#'   ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
#'   ndwi08
#' data(maipo)
#' out <- rf_cv_err(mtry = 3, ntree = 1000, train = train, test = test,
#' formula = fo, response = "croptype", rf_fun = "randomForest")
#'
#' @export
rf_cv_err <- function(ntree = NULL, mtry = NULL, train = NULL, test = NULL,
                      response = NULL, formula = NULL, rf_fun = NULL, ...) {

  if (rf_fun == "rfsrc") {
    args <- list(formula = formula, data = train, ntree = ntree, mtry = mtry)
  } else if (rf_fun == "randomForest") {
    args <- list(formula = formula, data = train, ntree = ntree, mtry = mtry)
  }
  # fit model
  fit <- do.call(rf_fun, args)

  # binary classifcation
  if (is.factor(train[[response]]) && length(levels(train[[response]])) == 2) {
    pred <- predict(fit, newdata = test, type = "prob")

    # calculate error measures
    if (rf_fun == "randomForest") {
      perf_measures <- err_default(test[, response], pred[, 2])
    } else if (rf_fun == "rfsrc") {
      perf_measures <- err_default(test[, response], pred$predicted[, 2])
    }
  }
  # multiclass classification
  else if (is.factor(train[[response]]) && length(levels(train[[response]])) > 2) {
    pred <- predict(fit, newdata = test)

    # calculate error measures
    if (rf_fun == "randomForest") {
      perf_measures <- err_default(test[, response], pred)
    } else if (rf_fun == "rfsrc") {
      perf_measures <- err_default(test[, response], pred$class)
    }
  }
  # regression
  else if (is.numeric(train[[response]])) {
    pred <- predict(fit, newdata = test)

    # calculate error measures
    if (rf_fun == "randomForest") {
      perf_measures <- err_default(test[, response], pred)
    } else if (rf_fun == "rfsrc") {
      perf_measures <- err_default(test[, response], pred$predicted)
    }
  }

  return(perf_measures)
}

#' @title check_response_type
#' @description Checks response type of input and sets error measure
#'
#' @param object fitted object
#'
#' @param error_measure optional user-defined error measure
#'
#' @keywords internal
#'
#'
#' @export
check_response_type <- function(object = NULL, error_measure = NULL,
                                perf_measures = NULL, option = FALSE) {

  # binary classification
  if (is.factor(object) &&
      length(levels(object)) == 2 |
      class(object) == "matrix") {
    if (is.null(error_measure)) {
      error_measure <- "auroc"
    }
    if (option == TRUE) {
      # get list index with highest auroc
      perf_measures %>%
        map(error_measure) %>%
        which.max() -> list_index
    }
  }
  # regression
  else if (is.numeric(object)) {
    if (is.null(error_measure)) {
      error_measure <- "rmse"
    }
    if (option == TRUE) {
      # get list index with lowest rmse
      perf_measures %>%
        map(error_measure) %>%
        which.min() -> list_index
    }
  }
  # multiclass classification
  else if (is.factor(object) &&
           length(levels(object)) > 2) {
    if (is.null(error_measure)) {
      error_measure <- "error"
    }
    if (option == TRUE) {
      # get list index with lowest rmse
      perf_measures %>%
        map(error_measure) %>%
        which.min() -> list_index
    }
  }
  if (option == TRUE) {
    return(list(error_measure, list_index))
  } else {
    return(error_measure)
  }
}
