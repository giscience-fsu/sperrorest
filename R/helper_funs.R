#' @title runfolds
#' @description Runs model fitting, error estimation and variable importance
#' on fold level
#'
#' @keywords internal
#' @importFrom purrr map
#' @export

runfolds <- function(j = NULL, current_sample = NULL, data = NULL, i = NULL,
                     formula = NULL, model_args = NULL, par_cl = NULL,
                     par_mode = NULL,
                     do_try = NULL, model_fun = NULL, error_fold = NULL,
                     error_rep = NULL, pred_fun = NULL, imp_variables = NULL,
                     imp_permutations = NULL, err_fun = NULL, train_fun = NULL,
                     err_train = NULL, importance = NULL, current_res = NULL,
                     current_impo = NULL, pred_args = NULL, progress = NULL,
                     pooled_obs_train = NULL, pooled_obs_test = NULL,
                     pooled_pred_train = NULL, response = NULL,
                     is_factor_prediction = NULL, pooled_pred_test = NULL,
                     coords = NULL, test_fun = NULL, imp_one_rep = NULL,
                     do_gc = NULL, test_param = NULL, train_param = NULL) {
  if (importance == FALSE) {
    if (par_mode == "foreach" | par_mode == "sequential" |
        par_mode == "foreach-old" && progress == "TRUE" | progress == 1) {
      cat(date(), "Repetition", i, "- Fold", j, "\n")
    }
  }

  # Create training sample:
  nd <- data[current_sample[[j]]$train, ]
  if (!is.null(train_fun))
    nd <- train_fun(data = nd, param = train_param)

  # Train model on training sample:
  margs <- c(list(formula = formula, data = nd), model_args)

  if (do_try) {
    fit <- try(do.call(model_fun, args = margs))

    # Error handling:
    if (class(fit) == "try-error") {
      fit <- NULL
      if (error_fold) {
        if (err_train) {
          current_res[[j]]$train <- NULL
          # res[[i]][[j]]$train = NULL
          current_res[[j]]$test <- NULL
          # res[[i]][[j]]$test =
        }
        if (importance) {
          current_impo[[j]] <- c()
        }
      }
      if (do_gc >= 2) {
        gc()
      }
      next  # skip this fold
    }

  } else {
    fit <- do.call(model_fun, args = margs)
  }

  if (err_train == TRUE) {
    # Apply model to training sample:
    pargs <- c(list(object = fit, newdata = nd), pred_args)
    if (is.null(pred_fun)) {
      pred_train <- do.call(predict, args = pargs)
    } else {
      pred_train <- do.call(pred_fun, args = pargs)
    }
    rm(pargs)

    # Calculate error measures on training sample:
    if (error_fold == TRUE) {
      if (do_try) {
        err_try <- try(err_fun(nd[, response], pred_train))
        if (class(err_try) == "try-error") {
          err_try <- NULL
        }
        current_res[[j]]$train <- err_try  #res[[i]][[j]]$train = err_try
      } else {
        current_res[[j]]$train <- err_fun(nd[, response], pred_train)
        #res[[i]][[j]]$train = err_fun(nd[,response], pred_train)
      }
    }
    if (error_rep == TRUE) {
      pooled_obs_train <- c(pooled_obs_train, nd[, response])
      pooled_pred_train <- c(pooled_pred_train, pred_train)
    }
  } else {
    if (error_fold == TRUE) {
      current_res[[j]]$train <- NULL  #res[[i]][[j]]$train = NULL
    }
  }

  # Create test sample:
  nd <- data[current_sample[[j]]$test, ]
  if (!is.null(test_fun)) {
    nd <- test_fun(data = nd, param = test_param)
  }
  # Create a 'backup' copy for variable importance assessment:
  if (importance) {
    nd_bak <- nd
  }
  # Apply model to test sample:
  pargs <- c(list(object = fit, newdata = nd), pred_args)
  if (is.null(pred_fun)) {
    pred_test <- do.call(predict, args = pargs)
  } else {
    pred_test <- do.call(pred_fun, args = pargs)
  }
  rm(pargs)

  # Calculate error measures on test sample:
  if (error_fold) {
    if (do_try) {
      err_try <- try(err_fun(nd[, response], pred_test))
      if (class(err_try) == "try-error") {
        err_try <- NULL
      }
      current_res[[j]]$test <- err_try  #res[[i]][[j]]$test = err_try
    } else {
      current_res[[j]]$test <- err_fun(nd[, response], pred_test)
      #res[[i]][[j]]$test  = err_fun(nd[,response], pred_test)
    }
  }
  if (error_rep) {
    pooled_obs_test <- c(pooled_obs_test, nd[, response])
    pooled_pred_test <- c(pooled_pred_test, pred_test)
    # assign to outer scope; otherwise object is NULL in runreps
    is_factor_prediction <<- is.factor(pred_test)
  }

  ### Permutation-based variable importance assessment:
  if (importance & error_fold) {
    if (is.null(current_res[[j]]$test)) {
      current_impo[[j]] <- c()
      if (!progress == FALSE) {
        # cat(date(), "-- skipping variable importance\n")
      }
    } else {
      if (!progress == FALSE) {
        # cat(date(), "-- Variable importance\n")
      }
      imp_temp <- imp_one_rep

      # Parallelize this: ???
      for (cnt in 1:imp_permutations) {
        # Some output on screen:
        if (!progress == FALSE & (cnt > 1)) {
          if (log10(cnt) == floor(log10(cnt))) {
            #cat(date(), "   ", cnt, "\n")
            cat(date(), "Repetition", names(current_sample)[i], "- Fold", j,
                "- permutation-count:", cnt, "\n")
          }
        }
        # Permutation indices:
        permut <- sample(1:nrow(nd), replace = FALSE)

        # For each variable:
        for (vnm in imp_variables) {
          # Get undisturbed backup copy of test sample:
          nd <- nd_bak
          # Permute variable vnm:
          nd[, vnm] <- nd[, vnm][permut]
          # Apply model to perturbed test sample:
          pargs <- c(list(object = fit, newdata = nd), pred_args)
          if (is.null(pred_fun)) {
            pred_test <- do.call(predict, args = pargs)
          } else {
            pred_test <- do.call(pred_fun, args = pargs)
          }
          rm(pargs)

          # Calculate variable importance:
          if (do_try) {
            permut_err <- try(err_fun(nd[, response], pred_test))
            if (class(permut_err) == "try-error") {
              imp_temp[[vnm]][[cnt]] <- c()  # ???
            } else {
              imp_temp[[vnm]][[cnt]] <- as.list(unlist(current_res[[j]]$test) -
                                                  unlist(permut_err))
              }
          } else {
            permut_err <- err_fun(nd[, response], pred_test)
            imp_temp[[vnm]][[cnt]] <- as.list(unlist(current_res[[j]]$test) -
                                                unlist(permut_err))
            }
        }
      }
      # average the results obtained in each permutation:
      current_impo[[j]] <- as.data.frame(t(sapply(imp_temp, function(y)
        sapply(as.data.frame(t(sapply(y, as.data.frame))), function(x)
          mean(unlist(x))))))
      rm(nd_bak, nd)  # better safe than sorry...
    }  # end of else if (!is.null(current_res[[j]]$test))
  }

  current_res <- current_res[[j]]
  current_impo <- current_impo[[j]]

  return(list(pooled_obs_train = pooled_obs_train,
              pooled_obs_test = pooled_obs_test,
              pooled_pred_train = pooled_pred_train,
              pooled_pred_test = pooled_pred_test,
              current_res = current_res,
              current_impo = current_impo))

}

#' @title runreps
#' @description Runs model fitting, error estimation and variable importance
#' on fold level
#' @keywords internal
#' @export
#'

# runreps function for lapply()
runreps <- function(current_sample = NULL, data = NULL, formula = NULL,
                    model_args = NULL, par_cl = NULL, do_gc = NULL,
                    imp_one_rep = NULL,
                    do_try = NULL, model_fun = NULL, error_fold = NULL,
                    error_rep = NULL, pred_fun = NULL, imp_variables = NULL,
                    imp_permutations = NULL, err_fun = NULL, train_fun = NULL,
                    err_train = NULL, importance = NULL, current_res = NULL,
                    current_impo = NULL, pred_args = NULL, progress = NULL,
                    pooled_obs_train = NULL, pooled_obs_test = NULL,
                    pooled_pred_train = NULL, response = NULL,
                    is_factor_prediction = NULL, pooled_pred_test = NULL,
                    coords = NULL, test_fun = NULL, par_mode = NULL, i = NULL) {
  # output data structures
  current_res <- NULL
  current_impo <- current_sample
  current_pooled_err <- NULL

  if (error_fold) {
    current_res <- lapply(current_sample, unclass)
    class(current_res) <- "sperroresterror"
  } else {
    current_res <- NULL
  }

  # Collect pooled results in these data structures:
  if (err_train) {
    pooled_obs_train <- pooled_pred_train <- c()
  }
  pooled_obs_test <- pooled_pred_test <- c()

  # this ensures that runfolds finds all objects which have been
  # defined until here
  environment(runfolds) <- environment()

  if (par_mode == "foreach" | par_mode == "sequential" &&
      progress == TRUE | progress == 2) {
    cat(date(), "Repetition", names(current_sample)[i], "\n")
  }

  map(seq_along(current_sample), function(rep)
    runfolds(j = rep, data = data, current_sample = current_sample,
             formula = formula, par_mode = par_mode, pred_fun = pred_fun,
             model_args = model_args, do_try = do_try, model_fun = model_fun,
             error_fold = error_fold, error_rep = error_rep,
             imp_permutations = imp_permutations,
             imp_variables = imp_variables,
             is_factor_prediction = is_factor_prediction,
             err_train = err_train, importance = importance,
             current_res = current_res,
             pred_args = pred_args, response = response, par_cl = par_cl,
             coords = coords, progress = progress,
             pooled_obs_train = pooled_obs_train,
             pooled_obs_test = pooled_obs_test,
             err_fun = err_fun)) -> runfolds_list

  # merge sublists of each fold into one list
  # http://stackoverflow.com/questions/32557131/adding-a-vector-to-each-sublist-within-a-list-r
  # http://stackoverflow.com/questions/43963683/r-flexible-passing-of-sublists-to-following-function
  runfolds_merged <- do.call(Map, c(f = list, runfolds_list))

  if (importance == TRUE) {
    # subset fold result to importance results only
    impo_only <- runfolds_merged[6][[1]]
    ### get mean from all impo results of all folds
    ### (multiple dataframes stored in a list)
    ### http://stackoverflow.com/questions/18371187/element-wise-mean-for-a-list-of-dataframes-with-na
    ### NICHT MITTELN, ENFACH ALLE IMPO (= FÜR JEDEN FOLD) ZURÜCKGEBEN
    # current_impo <- Reduce("+", impo_only) / length(impo_only)
  }

  pooled_only <- runfolds_merged[c(1:4)]
  pooled_only <- sapply(unique(names(pooled_only)), function(x)
    unname(unlist(pooled_only[names(pooled_only) == x])), simplify = FALSE)

  # Calculate error measures on pooled results
  if (error_rep) {
    if (is.factor(data[, response])) {
      lev <- levels(data[, response])
      if (err_train) {
        pooled_only$pooled_obs_train <- factor(lev[
          pooled_only$pooled_obs_train], levels = lev)
      }
      pooled_only$pooled_obs_test <- factor(lev[pooled_only$pooled_obs_test],
                                            levels = lev)
      if (is_factor_prediction) {
        if (err_train) {
          pooled_only$pooled_pred_train <- factor(lev[
            pooled_only$pooled_pred_train], levels = lev)
        }
        pooled_only$pooled_pred_test <- factor(lev[
          pooled_only$pooled_pred_test], levels = lev)
      }
    }
    pooled_err_train <- NULL
    if (err_train) {
      pooled_err_train <- err_fun(pooled_only$pooled_obs_train,
                                  pooled_only$pooled_pred_train)
    }

    list(train = pooled_err_train,
         test = err_fun(pooled_only$pooled_obs_test,
                        pooled_only$pooled_pred_test)) %>%
      unlist() %>%
      t() -> current_pooled_err

    if (do_gc >= 2) {
      gc()
    }
  }  # end for each fold

  if ((do_gc >= 1) & (do_gc < 2)) {
    gc()
  }

  # set current_impo to NULL to prevent false importance output (resamp object)
  # if not desired
  if (importance == FALSE) {
    impo_only <- NULL
  }

  return(list(error = runfolds_merged$current_res,
              pooled_err = current_pooled_err, importance = impo_only))
}


#' @title transfer_parallel_output
#' @description transfers output of parallel calls to runreps
#' @keywords internal
#' @export
transfer_parallel_output <- function(my_res = NULL, res = NULL, impo = NULL,
                                     pooled_err = NULL) {

  for (i in seq_along(my_res)) {
    if (i == 1) {
      pooled_err <- my_res[[i]]$pooled_err
      impo[[i]] <- my_res[[i]]$importance
      res[[i]] <- my_res[[i]]$error
    } else {
      pooled_err <- rbind(pooled_err, my_res[[i]]$pooled_err)
      impo[[i]] <- my_res[[i]]$importance
      res[[i]] <- my_res[[i]]$error
    }
  }

  return(list(pooled_err = pooled_err, impo = impo,
              res = res))
}
