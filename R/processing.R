#' @title runfolds
#' @description Runs model fitting, error estimation and variable importance on
#'   fold level
#'
#' @importFrom stats na.omit
#' @keywords internal
#' @importFrom stringr str_replace_all
#' @export
runfolds <- function(j = NULL,
                     current_sample = NULL,
                     data = NULL,
                     i = NULL,
                     formula = NULL,
                     model_args = NULL,
                     model_fun = NULL,
                     pred_fun = NULL,
                     imp_variables = NULL,
                     imp_permutations = NULL,
                     imp_sample_from = "test",
                     err_fun = NULL,
                     train_fun = NULL,
                     importance = NULL,
                     current_res = NULL,
                     current_impo = NULL,
                     pred_args = NULL,
                     pooled_obs_train = NULL,
                     pooled_obs_test = NULL,
                     pooled_pred_train = NULL,
                     response = NULL,
                     progress = NULL,
                     pooled_pred_test = NULL,
                     coords = NULL,
                     test_fun = NULL,
                     imp_one_rep = NULL,
                     do_gc = NULL,
                     test_param = NULL,
                     train_param = NULL) {

  if (importance == FALSE && progress == TRUE | progress == "fold") {
    cat(date(), "Repetition", i, "- Fold", j, "\n")
  }
  # set global variables for R CMD Check
  fold_number <- NULL
  repetition_number <- NULL

  # predictor variables:
  predvars <- all.vars(formula)[-1]

  # append fid to data to enable tracking of observations
  data$fid <- seq_len(nrow(data)) # nolint

  # Create training sample:
  nd_train <- data[current_sample[[j]]$train, ]
  if (!is.null(train_fun)) {
    nd_train <- train_fun(data = nd_train, param = train_param)
    current_sample[[j]]$train <- nd_train$fid
  }

  # Create test sample:
  nd_test <- data[current_sample[[j]]$test, ]

  # Prepare training args for training on training data
  margs <- c(list(formula = formula, data = nd_train), model_args)

  # initialize object
  not_converged_folds <- 0

  # tuning of ML models here

  # we do not see the error message, so returning none
  fit <- tryCatch(do.call(model_fun, args = margs),
                  error = function(cond) {
    return(NA)
  })

  # error handling for model fitting (e.g. maxent)
  # we need the first condition to handle S4 objects. They do not work with
  # is.na
  if (class(fit)[1] == "logical") {
    message(sprintf(
      paste0(
        "\n'sperrorest()': Non-convergence during model fit.", # nolint
        " Setting results of",
        " Repetition %s Fold %s to NA."
      ),
      i, j
    ))

    not_converged_folds <- not_converged_folds + 1

    return(list(
      pooled_obs_train = NA,
      pooled_obs_test = NA,
      pooled_pred_train = NA,
      pooled_pred_test = NA,
      current_res = NA,
      current_impo = NA,
      not_converged_folds = not_converged_folds,
      is_factor_prediction = NA
    ))
  }

  # Apply model to training sample:
  pargs <- c(list(object = fit, newdata = nd_train), pred_args)
  if (is.null(pred_fun)) {
    pred_train <- do.call(predict, args = pargs)
  } else {
    pred_train <- do.call(pred_fun, args = pargs)
  }
  rm(pargs)

  is_factor_prediction <- is.factor(pred_train)

  # Calculate error measures on training sample:

  if (any(class(nd_train) == "tbl")) {
    nd_train <- as.data.frame(nd_train) # nocov
  }
  current_res[[j]]$train <- tryCatch(err_fun(nd_train[, response], pred_train),
    error = function(cond) {
      return(NA)
    }
  )
  if (class(current_res[[j]]$train) == "logical") {
    message(sprintf(
      paste0(
        "\n'sperrorest()': Non-convergence during",
        " performance calculation.",
        " Setting results of",
        " Repetition %s Fold %s to NA."
      ),
      i, j
    ))

    not_converged_folds <- not_converged_folds + 1

    return(list(
      pooled_obs_train = NA,
      pooled_obs_test = NA,
      pooled_pred_train = NA,
      pooled_pred_test = NA,
      current_res = NA,
      current_impo = NA,
      not_converged_folds = not_converged_folds,
      resampling = current_sample,
      is_factor_prediction = is_factor_prediction
    ))
  }

  pooled_obs_train <- c(pooled_obs_train, nd_train[, response])
  pooled_pred_train <- c(pooled_pred_train, pred_train)

  if (!is.null(test_fun)) {
    nd_test <- test_fun(data = nd_test, param = test_param)
    current_sample[[j]]$test <- nd_test$fid
  }

  # Create a 'backup' copy for variable importance assessment:
  if (importance == TRUE) {
    nd_bak <- nd_test
  }

  # account for possible missing factor levels in test data
  if (any(class(fit) == "lm" | class(fit) == "glmmPQL")) {
    nd_test <- remove_missing_levels(fit, nd_test) # nolint
  }

  # remove NAs in test data.frame
  if (length(predvars) > 0) {
    if (length(predvars) == 1) {
      omit_na <- !is.na(nd_test[, predvars])
    } else {
      omit_na <- apply(nd_test[, predvars], 1, function(x) !any(is.na(x)))
    }
    nd_test <- nd_test[omit_na, ]
    ##nd_test <- na.omit(nd_test)
  }

  # Apply model to test sample:
  pargs <- c(list(object = fit, newdata = nd_test), pred_args)
  if (is.null(pred_fun)) {
    pred_test <- do.call(predict, args = pargs)
  } else {
    pred_test <- do.call(pred_fun, args = pargs)
  }
  rm(pargs)

  # Calculate error measures on test sample:
  if (inherits(nd_test, "tbl")) {
    nd_test <- as.data.frame(nd_test) # nocov
  }
  current_res[[j]]$test <- tryCatch(err_fun(nd_test[, response], pred_test),
    error = function(cond) {
      return(NA)
    }
  )

  # check if err_fun() errored and provide informative error message
  if (is.na(current_res[[j]]$test[1])) {

    message(sprintf(
      paste0(
        "err_fun(): Error in error estimation.",
        " Setting performance for repetition %s",
        " fold %s to NA.\n",
        " Classification: This most likely happens if the response of",
        " the test data does not contain all levels",
        " (due to spatial partitioning)",
        " and AUROC is used as the error measure.",
        " Try using a different number of folds or error measure."
      ),
      fold_number,
      repetition_number
    ))

    not_converged_folds <- not_converged_folds + 1

    return(list(
      pooled_obs_train = NA,
      pooled_obs_test = NA,
      pooled_pred_train = NA,
      pooled_pred_test = NA,
      current_res = NA,
      current_impo = NA,
      not_converged_folds = not_converged_folds,
      resampling = current_sample,
      is_factor_prediction = is_factor_prediction
    ))
  }

  pooled_obs_test <- c(pooled_obs_test, nd_test[, response])
  pooled_pred_test <- c(pooled_pred_test, pred_test)

  ### Permutation-based variable importance assessment:
  if (importance == TRUE) {

    # Get undisturbed backup copy of test sample:
    nd_test <- nd_bak

    # account for possible missing factor levels in test data
    if (any(class(fit) == "lm" | class(fit) == "glmmPQL")) {
      nd_test <- remove_missing_levels(fit, nd_test) # nolint
    }

    # remove NAs in test data.frame
    if (length(predvars) > 0) {
      if (length(predvars) == 1) {
        omit_na <- !is.na(nd_test[, predvars])
      } else {
        omit_na <- apply(nd_test[, predvars], 1, function(x) !any(is.na(x)))
      }
      nd_test <- nd_test[omit_na, ]
      ##nd_test <- na.omit(nd_test)
    }

    # Backup copy for permutation:
    nd_test_bak <- nd_test

    # does this ever happen??
    # nocov start
    if (is.null(current_res[[j]]$test) & (imp_sample_from == "test")) {
      current_impo[[j]] <- c()
      if (!progress == FALSE) {
        cat(date(), "-- skipping variable importance\n")
      } # nocov end
    } else {
      if (!progress == FALSE) {
        cat(date(), "-- Variable importance\n")
      }
      imp_temp <- imp_one_rep

      for (cnt in seq_len(imp_permutations)) {
        # Some output on screen:
        if (progress == "all" | progress == TRUE & (cnt > 1)) {
          if (log10(cnt) == floor(log10(cnt))) {
            cat(
              date(), "Repetition", i, "- Fold", j,
              "- permutation count:", cnt, "\n"
            )
          }
        }

        # Permutation data:
        if (imp_sample_from == "test") {
          nd_permute <- nd_test_bak
        } else if (imp_sample_from == "train") {
          nd_permute <- nd_train
        } else if (imp_sample_from == "all") {
          nd_permute <- data
        }
        # Permutation indices:
        replace <- (nrow(nd_permute) < nrow(nd_test))
        permut <- sample(seq_len(nrow(nd_permute)), replace = replace, size = nrow(nd_test))
        # Permutation data:
        nd_permute <- nd_permute[permut,]

        # For each variable:
        for (vnm in imp_variables) {

          # Get undisturbed backup copy of test sample:
          nd_test <- nd_test_bak

          # Get permuted variable vnm:
          nd_test[, vnm] <- nd_permute[, vnm]
          # Apply model to perturbed test sample:
          pargs <- c(list(object = fit, newdata = nd_test), pred_args)
          if (is.null(pred_fun)) {
            pred_test <- do.call(predict, args = pargs)
          } else {
            pred_test <- do.call(pred_fun, args = pargs)
          }
          rm(pargs)

          # Calculate variable importance:
          permut_err <- err_fun(nd_test[, response], pred_test)
          imp_temp[[vnm]][[cnt]] <- as.list(unlist(current_res[[j]]$test) -
            unlist(permut_err))

        }
      }
      # average the results obtained in each permutation:
      current_impo[[j]] <- as.data.frame(t(sapply(imp_temp, function(y) {
        sapply(as.data.frame(t(sapply(y, as.data.frame))), function(x) {
          mean(unlist(x))
        })
      })))
      rm(nd_bak, nd_test) # better safe than sorry...
    }
  }

  current_res <- current_res[[j]]
  current_impo <- current_impo[[j]]

  return(list(
    pooled_obs_train = pooled_obs_train,
    pooled_obs_test = pooled_obs_test,
    pooled_pred_train = pooled_pred_train,
    pooled_pred_test = pooled_pred_test,
    current_res = current_res,
    current_impo = current_impo,
    not_converged_folds = not_converged_folds,
    resampling = current_sample,
    is_factor_prediction = is_factor_prediction
  ))
}

#' @title runreps
#' @description Runs model fitting, error estimation and variable importance
#' on fold level
#' @keywords internal
#' @importFrom future.apply future_lapply
#' @export
#'

# runreps function for lapply()
runreps <- function(current_sample = NULL,
                    data = NULL,
                    formula = NULL,
                    model_args = NULL,
                    do_gc = NULL,
                    imp_one_rep = NULL,
                    model_fun = NULL,
                    pred_fun = NULL,
                    imp_variables = NULL,
                    imp_permutations = NULL,
                    imp_sample_from = "test",
                    err_fun = NULL,
                    importance = NULL,
                    current_res = NULL,
                    current_impo = NULL,
                    pred_args = NULL,
                    progress = NULL,
                    mode_fold = "sequential",
                    pooled_obs_train = NULL,
                    pooled_obs_test = NULL,
                    pooled_pred_train = NULL,
                    response = NULL,
                    pooled_pred_test = NULL,
                    test_fun = NULL,
                    test_param = NULL,
                    train_fun = NULL,
                    train_param = NULL,
                    coords = NULL,
                    i = NULL) {

  # output data structures
  current_res <- NULL
  current_impo <- current_sample
  current_pooled_error <- NULL

  current_res <- lapply(current_sample, unclass)
  class(current_res) <- "sperroresterror"

  # Collect pooled results in these data structures:
  pooled_obs_train <- pooled_pred_train <- c()
  pooled_obs_test <- pooled_pred_test <- c()

  # this ensures that runfolds finds all objects which have been
  # defined until here
  environment(runfolds) <- environment()

  if (progress == TRUE | progress == "rep") {
    cat(date(), "Repetition", i, "\n") # nocov
  }

  if (mode_fold == "sequential") {
    lapply(seq_along(current_sample), function(rep) {
      runfolds(
        j = rep,
        data = data,
        current_sample = current_sample,
        formula = formula,
        pred_fun = pred_fun,
        model_args = model_args,
        model_fun = model_fun,
        imp_permutations = imp_permutations,
        imp_variables = imp_variables,
        imp_sample_from = imp_sample_from,
        importance = importance,
        current_res = current_res,
        pred_args = pred_args,
        response = response,
        coords = coords,
        progress = progress,
        pooled_obs_train = pooled_obs_train,
        pooled_obs_test = pooled_obs_test,
        test_fun = test_fun,
        test_param = test_param,
        train_fun = train_fun,
        train_param = train_param,
        err_fun = err_fun
      )
    }) -> runfolds_list
  } else if (mode_fold == "future") {
    future.apply::future_lapply(seq_along(current_sample), function(rep) {
      runfolds(
        j = rep,
        data = data,
        current_sample = current_sample,
        formula = formula,
        pred_fun = pred_fun,
        model_args = model_args,
        model_fun = model_fun,
        imp_permutations = imp_permutations,
        imp_variables = imp_variables,
        imp_sample_from = imp_sample_from,
        importance = importance,
        current_res = current_res,
        pred_args = pred_args,
        response = response,
        coords = coords,
        progress = progress,
        pooled_obs_train = pooled_obs_train,
        pooled_obs_test = pooled_obs_test,
        test_fun = test_fun,
        test_param = test_param,
        train_fun = train_fun,
        train_param = train_param,
        err_fun = err_fun
      )
    },
    future.seed = TRUE
    ) -> runfolds_list
  } else if (mode_fold == "loop") {
    runfolds_list <- list()
    for (i_fold in seq_along(current_sample)) {
      runfolds_list[[i_fold]] <-
        runfolds(
          j = i_fold,
          data = data,
          current_sample = current_sample,
          formula = formula,
          pred_fun = pred_fun,
          model_args = model_args,
          model_fun = model_fun,
          imp_permutations = imp_permutations,
          imp_variables = imp_variables,
          imp_sample_from = imp_sample_from,
          importance = importance,
          current_res = current_res,
          pred_args = pred_args,
          response = response,
          coords = coords,
          progress = progress,
          pooled_obs_train = pooled_obs_train,
          pooled_obs_test = pooled_obs_test,
          test_fun = test_fun,
          test_param = test_param,
          train_fun = train_fun,
          train_param = train_param,
          err_fun = err_fun
        )
    }
  }

  is_factor_prediction <- sapply(runfolds_list, function(x) x$is_factor_prediction)
  is_factor_prediction <- is_factor_prediction[ !is.na(is_factor_prediction) ]
  if (length(is_factor_prediction) == 0) {
    is_factor_prediction <- FALSE
  } else {
    is_factor_prediction <- any(is_factor_prediction) # or all?
  }

  # merge sublists of each fold into one list
  # http://stackoverflow.com/questions/32557131/adding-a-vector-to-each-sublist-within-a-list-r # nolint
  # http://stackoverflow.com/questions/43963683/r-flexible-passing-of-sublists-to-following-function # nolint
  runfolds_merged <- do.call(Map, c(f = list, runfolds_list))

  if (all(is.na(runfolds_merged$pooled_obs_train))) {
    return(list(
      error = NA,
      pooled_error = NA, importance = NA,
      not_converged_folds = runfolds_merged$not_converged_folds,
      resampling = runfolds_merged$resampling
    ))
  }

  if (importance == TRUE) {
    # subset fold result to importance results only
    impo_only <- runfolds_merged[6][[1]]
  }

  pooled_only <- runfolds_merged[c(1:4)]
  pooled_only <- sapply(unique(names(pooled_only)), function(x) {
    unname(unlist(pooled_only[names(pooled_only) == x]))
  }, simplify = FALSE)

  if (any(class(data) == "tbl")) {
    data <- as.data.frame(data) # nocov
  }

  # Calculate error measures on pooled results
  if (is.factor(data[, response])) {
    lev <- levels(data[, response])
    pooled_only$pooled_obs_train <- factor(lev[
      pooled_only$pooled_obs_train
    ], levels = lev)
    pooled_only$pooled_obs_test <- factor(lev[pooled_only$pooled_obs_test],
      levels = lev
    )

    if (is_factor_prediction) {
      pooled_only$pooled_pred_train <- factor(lev[
        pooled_only$pooled_pred_train
      ], levels = lev)
      pooled_only$pooled_pred_test <- factor(lev[
        pooled_only$pooled_pred_test
      ], levels = lev)
    }

  }
  pooled_error_train <- NULL
  pooled_error_train <- err_fun(
    pooled_only$pooled_obs_train,
    pooled_only$pooled_pred_train
  )

  current_pooled_error <- t(unlist(list(
    train = pooled_error_train,
    test = err_fun(
      pooled_only$pooled_obs_test,
      pooled_only$pooled_pred_test
    )
  )))

  names <- str_replace_all(colnames(current_pooled_error), "[.]", "_")
  colnames(current_pooled_error) <- names

  if (do_gc >= 2) {
    gc() # nocov
  } # end for each fold

  if ((do_gc >= 1) & (do_gc < 2)) { # nolint
    gc() # nocov
  }

  # set current_impo to NULL to prevent false importance output (resamp object)
  # if not desired
  if (importance == FALSE) {
    impo_only <- NULL
  }

  return(list(
    error = runfolds_merged$current_res,
    pooled_error = current_pooled_error,
    importance = impo_only,
    not_converged_folds = runfolds_merged$not_converged_folds,
    resampling = runfolds_merged$resampling
  ))
}
