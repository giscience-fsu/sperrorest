context("sperrorest-summary.R")

Sys.setenv(R_TESTS = "")

pacman::p_load(sperrorest, rpart, testthat, MASS)

# sperrorest() binary response Wed Feb  8 21:40:49 2017 ------------------------

test_that("sperrorest() produces correct output for binary response", {

  #skip_on_os("mac") # don't know why summary tests failing on mac atm

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_fun = predict, pred_args = list(type = "response"),
                       par_args = list(par_mode = "foreach",
                                       par_units = 2),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 2))
  summary_rep <- summary(nspres$error_rep)
  summary_fold <- summary(nspres$error_fold)
  summary_resampling <- summary(nspres$represampling)

  expect_equal(length(nspres$error_rep[[1]]), 2) # reps
  expect_equal(length(nspres$error_fold[[1]]), 2) # folds
  expect_equal(length(summary_rep), 4) # binary response
  expect_equal(length(summary_fold), 4) # binary response
  expect_equal(length(summary_resampling), 2) # resampling summary
  # check for auroc existence
  expect_equal(names(nspres$error_rep)[[1]], "train_auroc")
})

# sperorrest() continuous response Wed Feb  8 22:19:57 2017 --------------------

test_that("sperrorest() produces correct output for binary response", {

  #skip_on_os("mac") # don't know why summary tests failing on mac atm

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope

  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm,
                       pred_fun = predict,
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 2),
                       par_args = list(par_mode = "foreach",
                                       par_units = 2),
                       benchmark = TRUE,
                       importance = TRUE, imp_permutations = 2)
  summary_rep <- summary(nspres$error_rep)
  summary_fold <- summary(nspres$error_fold)
  summary_resampling <- summary(nspres$represampling)

  expect_equal(length(nspres$error_rep[[1]]), 2) # reps
  expect_equal(length(nspres$error_fold[[1]]), 2) # folds
  expect_equal(length(summary_rep), 4) # binary response
  expect_equal(length(summary_fold), 4) # binary response
  expect_equal(length(summary_resampling), 2) # resampling summary
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
  # check for bias existence
  expect_equal(names(nspres$error_rep)[[1]], "train_bias")
})

# pred_fun = NULL response Wed Feb  8 22:19:57 2017 ----------------------------

test_that("sperrorest() produces correct output for binary response", {

  #skip_on_os("mac") # don't know why summary tests failing on mac atm

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope

  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm,
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 2),
                       par_args = list(par_mode = "foreach",
                                       par_units = 2),
                       importance = TRUE, imp_permutations = 2)

  expect_equal(length(nspres$error_rep[[1]]), 2) # reps
})

# summary.sperroresterror() Thu Feb  9 22:10:15 2017 ---------------------------

test_that("summary.sperroresterror() produces correct output for binary
          response", {

            #skip_on_os("mac") # don't know why summary tests failing on mac atm

            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slope ~ hcurv + vcurv + log.carea + cslope

            nspres <- sperrorest(data = ecuador, formula = fo,
                                 model_fun = glm,
                                 pred_fun = predict,
                                 smp_fun = partition_cv,
                                 par_args = list(par_mode = "foreach",
                                                 par_units = 2),
                                 smp_args = list(repetition = 1:2, nfold = 2))

            summary_rep1 <- summary(nspres$error_rep, pooled = FALSE)
            summary_fold1 <- summary(nspres$error_fold, pooled = FALSE)
            summary_rep <- summary(nspres$error_rep, pooled = TRUE)
            summary_fold <- summary(nspres$error_fold, pooled = TRUE)

            expect_equal(length(summary_rep), 4) # binary response
            expect_equal(length(summary_fold), 4) # binary response
          })

# summary.sperrorestimportance() Thu Feb  9 22:17:15 2017 ----------------------

test_that("summary.sperroresterror() with pooled = FALSE produces correct
          output for binary response", {

            #skip_on_os("mac") # don't know why summary tests failing on mac atm

            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slope ~ hcurv + vcurv + log.carea + cslope

            nspres <- sperrorest(data = ecuador, formula = fo,
                                 model_fun = glm,
                                 pred_fun = predict,
                                 smp_fun = partition_cv,
                                 par_args = list(par_mode = "foreach",
                                                 par_units = 2),
                                 smp_args = list(repetition = 1:2, nfold = 2),
                                 importance = TRUE, imp_permutations = 2)

            summary_imp <- summary(nspres$importance)

            expect_equal(length(summary_imp), 28)
          })

# sperrorest depr. args Thu Feb  9 22:42:48 2017 ------------------------------

test_that("deprecated args", {

  #skip_on_os("mac") # don't know why summary tests failing on mac atm

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope

  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv,
                          smp_args = list(repetition = 1:2, nfold = 2),
                          predfun = NULL))

  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv,
                          smp_args = list(repetition = 1:2, nfold = 2),
                          silent = NULL))

  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv,
                          smp_args = list(repetition = 1:2, nfold = 2),
                          err.pooled = NULL))
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv,
                          smp_args = list(repetition = 1:2, nfold = 2),
                          err.unpooled = NULL))
})

# sperrorest() various non default arguments Fri Feb 10 19:09:01 2017 ----------

# does not work with err_train = F and importance = T

test_that("sperrorest() produces correct output for binary response for
          non-default arguments", {

            #skip_on_os("mac") # don't know why summary tests failing on mac atm

            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            nspres <- sperrorest(data = ecuador, formula = fo,
                                 model_fun = glm,
                                 model_args = list(family = "binomial"),
                                 pred_fun = predict,
                                 pred_args = list(type = "response"),
                                 par_args = list(par_mode = "foreach",
                                                 par_units = 2),
                                 smp_fun = partition_cv,
                                 smp_args = list(repetition = 1:2, nfold = 2),
                                 benchmark = F,
                                 importance = TRUE, imp_permutations = 2,
                                 do_gc = 2)

            expect_equal(length(nspres$error_rep[[1]]), 2) # reps
            expect_equal(length(nspres$error_fold[[1]]), 2) # folds
            expect_equal(length(nspres$importance[[1]]), 2) # import folds
            expect_equal(length(nspres$importance), 2) # import reps
          })

# summary.sperrorest() Sun Feb 12 11:56:13 2017 ------------------------------

test_that("summary.sperrorest() works correctly", {

  #skip_on_os("mac") # don't know why summary tests failing on mac atm

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope

  out <- sperrorest(data = ecuador, formula = fo,
                    model_fun = glm,
                    pred_fun = predict,
                    par_args = list(par_mode = "foreach",
                                    par_units = 2),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 2),
                    importance = T, imp_permutations = 2, benchmark = T)

  smry_out <- summary(out)

  expect_equal(length(smry_out), 6)
})

test_that("is_factor_prediction object for classification models", {

  #skip_on_os("mac") # don't know why summary tests failing on mac atm

  testthat::skip_on_cran()

  lda_predfun <- function(object, newdata, fac = NULL) {
    library(nnet)
    majority <- function(x) {
      levels(x)[which.is.max(table(x))]
    }

    majority_filter <- function(x, fac) {
      for (lev in levels(fac)) {
        x[ fac == lev ] <- majority(x[ fac == lev ])
      }
      x
    }

    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[, fac])
    return(pred)
  }

  fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 + b24 +
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
    ndwi08

  data(maipo)

  # error_rep = TRUE, error_fold = TRUE
  out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                    model_fun = lda,
                    pred_fun = lda_predfun,
                    par_args = list(par_mode = "foreach",
                                    par_units = 2),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 2),
                    benchmark = FALSE, progress = FALSE)

  smry_out <- summary(out)

  expect_equal(length(smry_out), 6)
})
