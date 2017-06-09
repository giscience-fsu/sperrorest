#' @title svm_cv_err
#' @description Calculates AUROC for different gamma and cost values
#'
#' @importFrom ROCR prediction performance
#' @import e1071
#' @importFrom gmum.r SVM
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
#' response <- "slides"
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' out <- svm_cv_err(cost = 0.01, gamma = 0.166, train = train, test = test,
#' formula = fo, kernel = "radial", type = "C-classification", response = "slides",
#' svm_function = "svm")
#'
#' @export
svm_cv_err <- function(cost = NULL, gamma = NULL, train = NULL, test = NULL,
                       response = NULL, formula = NULL, kernel = NULL,
                       type = NULL, svm_fun = NULL, ...) {
  err <- c()

  # check type of response variable
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

    if (svm_fun == "ksvm") {
      pred <- predict(fit, newdata = test, type = "probabilities")
    } else {
      pred <- predict(fit, newdata = test, probability = TRUE)
    }

    if (!svm_fun == "ksvm") {
      pred <-  attr(pred, "probabilities")[, 2]
      predobj <- prediction(pred, test[, response])
    } else {
      predobj <- prediction(pred[, 2], test[, response])
    }

    auroc <- performance(predobj, measure = "auc")@y.values[[1]]

    err <- c(err, auroc)

    return(mean(err))
  }
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
