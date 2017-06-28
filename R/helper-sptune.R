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
#' \dontrun{
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
#' parti <- partition_kmeans(maipo, nfold = 5,
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
#' }
#' @export
svm_cv_err <- function(parameter1 = NULL, parameter2 = NULL,
                       train = NULL, test = NULL,
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
                 prob.model = prob.model, C = parameter1, gamma = parameter2)
  } else if (svm_fun == "SVM") {
    args <- list(formula = formula, data = train, type = type,
                 kernel = kernel, probability = probability, C = parameter1,
                 gamma = parameter2)
  } else if (svm_fun == "svm") {
    args <- list(formula = formula, data = train, type = type,
                 kernel = kernel, probability = probability, cost = parameter1,
                 gamma = parameter2)
  }
  fit <- try(do.call(svm_fun, args))

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
  if (class(pred) == "logical") {
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
#' \dontrun{
#' ##------------------------------------------------------------
#' ## binary classification
#' ##------------------------------------------------------------
#'
#' parti <- partition_kmeans(ecuador, nfold = 5)
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
#' parti <- partition_kmeans(maipo, nfold = 5,
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
#' }
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

#' @title maxent_cv_err
#' @description Tunes maxent hyperparameters beta_multiplier and
#' feature_classes using cross-validation with AUROC as error measure
#'
#' @importFrom ROCR prediction performance
#' @importFrom dismo maxent
#' @importFrom dplyr semi_join
#'
#' @param formula model formula
#'
#' @param beta_multiplier optional user-defined vector of 'beta_multiplier'
#' hyperparameter to tune over. See details.
#'
#' @param feature_classes optional user-defined vector of 'feature_classes'
#' hyperparameter to tune over. See details.
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
#' \dontrun{
#' data(maxent_pred)
#' data(maxent_response)
#'
#' parti <- partition_kmeans(basque, nfold = 5)
#' train <- maxent_pred[parti[[1]][[1]]$train, ]
#' test <- maxent_pred[parti[[1]][[1]]$test, ]
#'
#' out <- maxent_cv_err(beta_multiplier = 1,
#'                      feature_classes = "LQ",
#'                      train = train, test = test, x = maxent_pred,
#'                      p = maxent_response, response = "diplo01",
#'                      absence = TRUE)
#' }
#' @export
maxent_cv_err <- function(beta_multiplier = NULL, feature_classes = NULL,
                          train = NULL, test = NULL, x = NULL, p = NULL,
                          absence = NULL, ...) {

  # account for feature classes
  if (feature_classes == "L") {
    linear <- TRUE
    quadratic <- FALSE
    product <- FALSE
    threshold <- FALSE
    hinge <- FALSE
  } else if (feature_classes == "Q") {
    linear <- FALSE
    quadratic <- TRUE
    product <- FALSE
    threshold <- FALSE
    hinge <- FALSE
  } else if (feature_classes == "H") {
    linear <- FALSE
    quadratic <- FALSE
    product <- FALSE
    threshold <- FALSE
    hinge <- TRUE
  } else if (feature_classes == "T") {
    linear <- FALSE
    quadratic <- FALSE
    product <- FALSE
    threshold <- TRUE
    hinge <- FALSE
  } else if (feature_classes == "LQ") {
    linear <- TRUE
    quadratic <- TRUE
    product <- FALSE
    threshold <- FALSE
    hinge <- FALSE
  } else if (feature_classes == "HQ") {
    linear <- FALSE
    quadratic <- TRUE
    product <- FALSE
    threshold <- FALSE
    hinge <- TRUE
  } else if (feature_classes == "LQP") {
    linear <- TRUE
    quadratic <- TRUE
    product <- TRUE
    threshold <- FALSE
    hinge <- FALSE
  } else if (feature_classes == "LQT") {
    linear <- TRUE
    quadratic <- TRUE
    product <- FALSE
    threshold <- TRUE
    hinge <- FALSE
  } else if (feature_classes == "QHP") {
    linear <- FALSE
    quadratic <- TRUE
    product <- TRUE
    threshold <- FALSE
    hinge <- TRUE
  } else if (feature_classes == "QHT") {
    linear <- FALSE
    quadratic <- TRUE
    product <- FALSE
    threshold <- TRUE
    hinge <- TRUE
  } else if (feature_classes == "QHPT") {
    linear <- FALSE
    quadratic <- TRUE
    product <- TRUE
    threshold <- TRUE
    hinge <- TRUE
  }

  sprintf(paste0("betamultiplier=%s,autofeature=FALSE,",
                 "linear=%s,quadratic=%s,product=%s,threshold=%s,",
                 "hinge=%s"), beta_multiplier, linear, quadratic, product,
          threshold, hinge) -> my_args
  str_split(my_args, pattern = ",")[[1]] -> my_args

  # subset response vector for train and test
  x$pa <- p
  train_tmp <- suppressMessages(semi_join(x, train))
  p_train <- train_tmp$pa

  test_tmp <- suppressMessages(semi_join(x, test))
  p_test <- test_tmp$pa

  args <- list(x = train, p = p_train, args = my_args)

  # fit model
  fit <- tryCatch(do.call(maxent, args),
                  error = function(cond) {
                    return(NA)
                  })

  # if NA is assigned to 'fit' due to tryCatch, we break the function and
  # return NA
  if (is.logical(fit)) {
    return(fit)
  }

  # p must be numeric here
  prev_pa <- mean(as.numeric(as.character(p_train)))

  # post-process predict results http://onlinelibrary.wiley.com/store/10.1111/2041-210X.12252/asset/supinfo/mee312252-sup-0003-Appendix5.R?v=1&s=13755a940831aa9186cf931209e826a304e9868e
  if (absence == TRUE) {
    pred <- tryCatch(dismo::predict(fit, x = test, args = "outputformat=raw"),
                     error = function(cond) {
                       return(NA)
                     })
    numbg <- fit@results["X.Background.points", ] # number of points in background
    pred <- pred * prev_pa * numbg # adjusted raw output (probabilities)
    pred[pred > 1] <- 1   # clipped to 1
  } else {
    # return pred for presence-only in logistic format
    pred <- tryCatch(dismo::predict(fit, x = test,
                                    args = "outputformat=logistic"),
                     error = function(cond) {
                       return(NA)
                     })
  }

  # if NA is assigned to 'pred' due to tryCatch, we break the function and
  # return NA
  if (is.logical(pred)) {
    return(pred)
  }

  # calculate error measures
  perf_measures <- err_default(p_test, pred)

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
