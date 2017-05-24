#' @title svm_tuning
#' @description Tuning of SVM (cost & gamma) using spatial cross-validation
#' 
#' @import future
#' @import doFuture
#' @import parallel
#' @import foreach
#' 
#' @param formula
#' 
#' @param data
#' 
#' @param accelerate option to speed up tuning using less cost and gamma values. 
#' Use `accelerate = 2` or `accelerate = 4` for test runs, but `accelerate = 1` 
#' for actual analysis.
#' 
#' @param int_cv_fold number of folds for cross-validation
#' 
#' @param partition.fun method for partitioning the data (e.g. [partition.kmeans])
#' 
#' @param additional options passed to [SVM]
#' 
#' @details This function tunes a support vectort machine from the [e1071] package 
#' using (spatial) cross-validation.
#' 
#' Currently this function is hard-coded to a binary response variable and AUROC 
#' as error measure. 
#' 
#' @examples 
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
#' svm_tune <- svm_tuning(fo, ecuador, accelerate = 4, int.cv.nfold = 5, partition.fun = partition.kmeans) 
#' 
#' @export
svm_tuning <- function(formula, data, accelerate = 1, int.cv.nfold = NULL, partition.fun = NULL,
                       out.progress = "", kernel = "radial", ...) {
  
  partition.fun_arg <- as.character(quote(partition.fun))
  
  if (is.null(partition.fun)) {
    message(sprintf("Using %s as partitioning method", partition.fun_arg))
    partition.fun <- partition.kmeans
  } else {
    message(sprintf("Using %s as partitioning method", partition.fun_arg))
  }
  
  if (is.null(int.cv.nfold)) {
    int.cv.nfold <- 5
    warning(sprintf("Using %s folds since 'int.cv.fold' was not set.", int.cv.nfold))
  }
  
  lhs <- as.character(formula)[2]
  
  parti <- partition.fun(data, nfold = int.cv.nfold, order.clusters = FALSE)
  train <- data[parti[[1]][[1]]$train, ]
  test <- data[parti[[1]][[1]]$test, ]
  
  # Perform a complete grid search over the following range of values:
  costs <- 10^seq(-2, 4, by = 0.5 * accelerate)
  default.gamma <- 1 / length(strsplit(as.character(formula)[3], "+", fixed = TRUE)[[1]])
  gammas <- unique( c(default.gamma, 10^seq(-4, 1, by = 0.5 * accelerate)))
  
  # Set up variables for loop:
  n.costs <- length(costs)
  costs <- rep(costs, length(gammas))
  gammas <- rep(gammas, each = n.costs)
  auroc <- rep(NA, length(costs))
  
  # Calculate AUROC for all combinations of cost and gamma values:
  
  for (i in 1:length(costs)) {
    auroc[i] <- svm_cv_err(cost = costs[i], gamma = gammas[i], train = train,
                           test = test, lhs = lhs, formula = formula,  
                           kernel = kernel, ...)   
  } 
  
  # Identify best AUROC, or if all are NA, use defaults and issue a warning:
  if (all(is.na(auroc))) {
    cost <- 1
    gamma <- default.gamma
    warning("all AUROCs are NA in internal cross-validation")
  } else {
    wh <- which(auroc == max(auroc, na.rm = TRUE))[1]
    cost <- costs[wh]
    gamma <- gammas[wh]
  }
  
  # Output on screen:
  cat("Optimal C: ", cost, "        optimal gamma: ", 
      gamma,"       optimal auroc: ", 
      max(auroc,na.rm = T), "\n")
  
  # Generate the actual fit object using optimized cost and gamma parameters:
  fit <- svm(formula, data,
             type = "C-classification",
             probability = TRUE,
             cost = cost, gamma = gamma)
  
  # Keep track of optimal cost and gamma values:
  fit$my.cost <- cost
  fit$my.costs <- costs
  fit$my.gamma <- gamma
  fit$my.gammas <- gammas
  
  return(fit)
}
