#' @title svm_cv_err
#' @description Calculates AUROC for different gamma and cost values
#' 
#' @importFrom ROCR prediction performance
#' @import e1071  
#' @importFrom gmum.r SVM
#' @importFrom kernlab ksvm predict
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
#' parti <- partition.kmeans(ecuador, nfold = 5, order.clusters = FALSE)
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
      pred <- kernlab::predict(fit, newdata = test, type = "probabilities")
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
#' parti <- partition.kmeans(ecuador, nfold = 5, order.clusters = FALSE)
#' train <- ecuador[parti[[1]][[1]]$train, ]
#' test <- ecuador[parti[[1]][[1]]$test, ]
#' response <- "slides"
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' out <- rf_cv_err(mtry = 3, ntree = 1000, train = train, test = test,
#' formula = fo, response = "slides", rf_function = "randomForest")
#' 
#' @export
rf_cv_err <- function(ntree = NULL, mtry = NULL, train = NULL, test = NULL,
                      response = NULL, formula = NULL, rf_fun = NULL, ...) { 
  err <- c()
  
  # check type of response variable 
  if (is.factor(train[[response]])) {
    
    if (rf_fun == "rfsrc") {
      args <- list(formula = formula, data = train, ntree = ntree, mtry = mtry)
      
    }
    if (rf_fun == "randomForest") {
      args <- list(formula = formula, data = train, ntree = ntree, mtry = mtry)
    } 
    fit <- do.call(rf_fun, args)
    
    if (rf_fun == "randomForest") {
      pred <- predict(fit, newdata = test, type = "prob")
    } else {
      pred <- predict(fit, newdata = test)
    }
    if (rf_fun == "randomForest") {
      predobj <- prediction(pred[, 2], test[, response])
    } else {
      pred <-  attr(pred, "probabilities")[, 2]
      predobj <- prediction(pred, test[, response]) 
    }  
    
    auroc <- performance(predobj, measure = "auc")@y.values[[1]]
    
    err <- c(err, auroc)
    
    return(mean(err))
  }
}
