#' @title svm_cv_err
#' @description Calculates AUROC for different gamma and cost values
#' 
#' @importFrom ROCR prediction performance
#' @importFrom e1071 svm
#' 
#' @param cost svm cost value
#' @param gamma svm gamma value
#' 
#' @keywords internal
#' @export
svm_cv_err <- function(cost = NULL, gamma = NULL, train = NULL, test = NULL,
                       lhs = NULL, formula = NULL, kernel = NULL, ...) {
  err <- c()
  
  # using the "formula" class here
  fit <- svm(formula = formula, data = train, kernel = kernel, 
             type = "C-classification", probability = TRUE,
             cost = cost, gamma = gamma, ...)
  
  pred <- predict(fit, newdata = test, probability = TRUE)
  pred <-  attr(pred, "probabilities")[, 2]
  predobj <- prediction(pred, test[, lhs])
  
  auroc <- performance(predobj, measure = "auc")@y.values[[1]]

  #cat(auroc, "\n")
  err <- c(err, auroc)
  
  return(mean(err))
}
