#' @title Default error function
#'
#' @description Calculate a variety of accuracy measures from observations and predictions
#' of numerical and categorical response variables.
#'
#' @name err_default
#'
#' @importFrom ROCR prediction performance
#'
#' @param obs factor, logical, or numeric vector with observations
#'
#' @param pred factor, logical, or numeric vector with predictions. Must be of
#' same type as `obs` with the exception that `pred` may be numeric
#' if `obs` is `factor` or `logical` ('soft' classification).
#'
#' @return A list with (currently) the following components, depending on the
#' type of prediction problem:
#'
#' \item{'hard' classification}{misclassification error, overall accuracy;
#' if two classes, sensitivity, specificity, positive predictive value (PPV),
#' negative predictive value (NPV), kappa}
#' \item{'soft' classification}{area under the ROC curve, error and accuracy
#' at a obs>0.5 dichotomization, false-positive rate (FPR; 1-specificity)
#' at 70, 80 and 90 percent sensitivity, true-positive rate (sensitivity)
#' at 80, 90 and 95 percent specificity}
#' \item{regression}{bias, standard deviation, mean squared error,
#' MAD ([mad]), median, interquartile range ([IQR])
#' of residuals}
#'
#' @note `NA` values are currently not handled by this function,
#' i.e. they will result in an error.
#'
#' @seealso \pkg{ROCR}
#'
#' @examples
#' obs <- rnorm(1000)
#' # Two mock (soft) classification examples:
#' err_default( obs > 0, rnorm(1000) ) # just noise
#' err_default( obs > 0, obs + rnorm(1000) ) # some discrimination
#' # Three mock regression examples:
#' err_default( obs, rnorm(1000) ) # just noise, but no bias
#' err_default( obs, obs + rnorm(1000) ) # some association, no bias
#' err_default( obs, obs + 1 ) # perfect correlation, but with bias
#'
#' @export
err_default <- function(obs, pred) {
  # The following wrapper functions are used in order to avoid warning messages:
  mmin <- function(x, ...) {
    if (length(x) == 0) {
      x <- Inf # nocov
    }
    return(min(x, ...))
  }
  mmax <- function(x, ...) {
    if (length(x) == 0) {
      x <- -Inf # nocov
    }
    return(max(x, ...))
  }

  # Convert logical to factor if necessary:
  if (is.logical(obs)) {
    obs <- factor(obs, levels = c("FALSE", "TRUE")) # nocov
  }
  if (is.logical(pred)) {
    pred <- factor(pred, levels = c("FALSE", "TRUE")) # nocov
  }

  # Classification problem:
  if (is.factor(obs)) {
    if (is.factor(pred)) {
      # 'hard' classification:
      pred <- as.character(pred)
      pred <- factor(pred, levels = levels(obs))
      err <- list(error = mean(obs != pred), accuracy = mean(obs == pred))
      # binary classification without probabilities
      if (nlevels(obs) == 2) {
        npos <- sum(obs == levels(obs)[2])
        nneg <- sum(obs == levels(obs)[1])
        ntruepos <- sum((obs == levels(obs)[2]) & (pred == levels(obs)[2])) # nolint
        ntrueneg <- sum((obs == levels(obs)[1]) & (pred == levels(obs)[1])) # nolint
        err$sensitivity <- ntruepos / npos
        err$specificity <- ntrueneg / nneg
        npospred <- sum(pred == levels(obs)[2])
        nnegpred <- sum(pred == levels(obs)[1])
        err$ppv <- ntruepos / npospred
        err$npv <- ntrueneg / nnegpred
        n <- length(obs)
        pexp <- (npos / n) * (npospred / n) + (nneg / n) * (nnegpred / n)
        if (pexp == 1) {
          err$kappa <- NA
        } else err$kappa <- (err$accuracy - pexp) / (1 - pexp)
      }
    } else {
      # 'soft' classification: Calculate area under the ROC curve:
      predobj <- prediction(pred, obs)
      auroc <- performance(predobj, measure = "auc")@y.values[[1]]
      err <- list(auroc = auroc)

      pos <- levels(obs)[2]
      # neg <- levels(obs)[1] # not in use

      err$error <- mean((obs == pos) != (pred >= 0.5)) # nolint
      err$accuracy <- 1 - err$error

      # internal functions for calculating false positive rate (1-specificity)
      # and true positive rate (sensitivity) for a given decision threshold:
      tpr <- function(o, p, np, t) sum(o & (p >= t)) / np
      fpr <- function(o, p, nn, t) sum(!o & (p >= t)) / nn

      # Number of positive and negative predictions:
      npos <- sum(obs == pos)
      nneg <- sum(obs != pos)

      err$sensitivity <- tpr(obs == pos, pred, npos, 0.5)
      err$specificity <- 1 - fpr(obs == pos, pred, nneg, 0.5)

      thrs <- unique(pred)

      if (length(thrs) > 500)
        thrs <- seq(min(pred) + 1e-04, max(pred) + 1e-04, length = 500)

      thr <- mmax(thrs[sapply(thrs, function(x) tpr(obs == pos, pred, npos,
                                                    x) >= 0.7)])
      err$fpr70 <- fpr(obs == pos, pred, nneg, thr)
      thr <- mmax(thrs[sapply(thrs, function(x) tpr(obs == pos, pred, npos,
                                                    x) >= 0.8)])
      err$fpr80 <- fpr(obs == pos, pred, nneg, thr)
      thr <- mmax(thrs[sapply(thrs, function(x) tpr(obs == pos, pred, npos,
                                                    x) >= 0.9)])
      err$fpr90 <- fpr(obs == pos, pred, nneg, thr)

      thr <- mmin(thrs[sapply(thrs, function(x) fpr(obs == pos, pred, nneg,
                                                    x) <= 0.2)])
      err$tpr80 <- tpr(obs == pos, pred, npos, thr)
      thr <- mmin(thrs[sapply(thrs, function(x) fpr(obs == pos, pred, nneg,
                                                    x) <= 0.1)])
      err$tpr90 <- tpr(obs == pos, pred, npos, thr)
      thr <- mmin(thrs[sapply(thrs, function(x) fpr(obs == pos, pred, nneg,
                                                    x) <= 0.05)])
      err$tpr95 <- tpr(obs == pos, pred, npos, thr)
    }
    err$events <- sum(obs == levels(obs)[2])
  } else {
    # Regression problem:
    err <- list(bias = mean(obs - pred), stddev = sd(obs - pred),
                mse = mean((obs - pred) ^ 2), mad = mad(obs - pred),
                median = median(obs - pred), iqr = IQR(obs - pred))
  }
  # Number of observations available:
  err$count <- length(obs)

  return(err)
}
