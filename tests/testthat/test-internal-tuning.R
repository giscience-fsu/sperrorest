context("sptune_svm")

Sys.setenv(R_TESTS = "")

pacman::p_load(testthat, sperrorest, kernlab, e1071, purrr,
               magrittr, randomForestSRC)

test_that("Internal tuning works with svm()", {

  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  svm_predfun <- function(object, newdata) {
    pred <- predict(object, newdata = newdata, probability = TRUE)
    pred <- attr(pred, "probabilities")[, 2]
  }

  par_nsp_res <- sperrorest(data = ecuador, formula = fo,
                            model_fun = svm,
                            pred_fun = svm_predfun,
                            progress = TRUE,
                            smp_fun = partition_cv,
                            smp_args = list(repetition = 1:2,
                                            nfold = 2),
                            par_args = list(par_mode = "foreach"),
                            tune = TRUE,
                            tune_args = list(accelerate = 16,
                                             kernel = "radial",
                                             type = "C-classification",
                                             model_fun = "svm"),
                            benchmark = TRUE)

  expect_equal(length(par_nsp_res$error_fold[[1]]), 2)
})

# rfrsc Wed Jun 28 17:28:21 2017 ------------------------------

test_that("Internal tuning works with rfsrc()", {

  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  rf_src_predfun <- function(object, newdata, type = NULL) {
    pred <- predict(object = object, newdata = newdata, type = type)
    pred <- pred$predicted[, 2]
  }

  par_nsp_res <- sperrorest(formula = fo, data = ecuador, model_fun = rfsrc,
                            pred_fun = rf_src_predfun,
                            pred_args = list(type = "prob"),
                            par_args = list(par_mode = "foreach"),
                            smp_fun = partition_kmeans, progress = TRUE,
                            smp_args = list(repetition = 2, nfold = 2),
                            tune = TRUE,
                            tune_args = list(step_factor = 32,
                                             model_fun = "rfsrc"),
                            benchmark = TRUE)

  expect_equal(length(par_nsp_res$error_fold[[1]]), 2)
})
