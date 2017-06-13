context("sperrorest.R")

Sys.setenv(R_TESTS = "")

pacman::p_load(sperrorest, testthat, rpart, MASS, foreach, doFuture, future,
               pbmcapply, magrittr)

# par_mode = "foreach" Mon Feb  6 23:24:11 2017 --------------------------

test_that("output type (= list) for different logical combinations of
          error_rep and error_fold for par_mode = 'foreach' on LDA example", {

            skip_on_cran()
            skip_on_os("mac") # don't know why this tests fails on travis (mac)

            lda_predfun <- function(object, newdata, fac = NULL) {
              library(nnet)
              majority <- function(x) {
                levels(x)[which.is.max(table(x))]
              }

              majority_filter <- function(x, fac) {
                for (lev in levels(fac)) {
                  x[fac == lev] <- majority(x[fac == lev])
                }
                x
              }

              pred <- predict(object, newdata = newdata)$class
              if (!is.null(fac)) pred <- majority_filter(pred, newdata[, fac])
              return(pred)
            }

            fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
              b24 +
              b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
              b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
              b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
              b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
              ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
              ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
              ndwi08

            data(maipo)

            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda_predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 4),
                              benchmark = TRUE, progress = 2)

            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "list")
            # check that train_error is first
            expect_equal(names(out$error_rep)[[1]], "train_error")
          })

test_that("output length of list is correct for par_mode = 'foreach' on rpart
          example", {

            skip_on_cran()

            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            mypred_rpart <- function(object, newdata) predict(object,
                                                              newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)

            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred_rpart <- function(object, newdata) predict(object,
                                                              newdata)[,2]

            out <- sperrorest(data = ecuador, formula = fo,
                              model_fun = rpart,
                              model_args = list(control = ctrl),
                              pred_fun = mypred_rpart,
                              progress = FALSE,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2,
                                              nfold = 2),
                              par_args = list(par_mode = "foreach",
                                              par_units = 2))

            expect_equal(length(out$error_fold[[1]]), 2)
          })

test_that("output length of list is correct for error_rep = TRUE and
          error_fold  = TRUE for par_mode = 'foreach' on svm example", {

            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            svm_predfun <- function(object, newdata) {
              pred <- predict(object, newdata = newdata, probability = TRUE)
              pred <- attr(pred, "probabilities")[, 2]
            }

            par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                                      model_fun = svm,
                                      model_args = list(cost = 10000, gamma = 0.0001,
                                                        kernel = "sigmoid",
                                                        probability = TRUE),
                                      pred_fun = svm_predfun,
                                      progress = TRUE,
                                      smp_fun = partition_cv,
                                      smp_args = list(repetition = 1:2,
                                                      nfold = 2),
                                      par_args = list(par_mode = "foreach"),
                                      error_rep = TRUE, error_fold = TRUE)

            expect_equal(length(par.nsp.res$error_fold[[1]]), 2)
          })

test_that("output length of list is correct for error_rep = TRUE and
          error_fold  = TRUE for par_mode = 'foreach' on svm example", {

            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            rf_spcv <- sperrorest(formula = fo, data = df,
                                  model_fun = randomForest,
                                  model_args = list(ntree = 1900,
                                                    mtry = 9),
                                  pred_args = list(type = "prob"),
                                  par_args = list(par_mode = "foreach"),
                                  smp_fun = partition_kmeans, progress = 2,
                                  smp_args = list(repetition = 1:4, nfold = 5))

            expect_equal(length(par.nsp.res$error_fold[[1]]), 2)
          })

# variable importance Wed Feb  8 21:59:03 2017

test_that("sperrorest() variable importance (foreach)", {

            skip_on_cran()

            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            out <- sperrorest(data = ecuador, formula = fo,
                              model_fun = glm,
                              model_args = list(family = "binomial"),
                              pred_fun = predict,
                              pred_args = list(type = "response"),
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:1, nfold = 4),
                              par_args = list(par_mode = "foreach",
                                              par_units = 2),
                              benchmark = TRUE,
                              importance = TRUE, imp_permutations = 10)
            expect_equal(class(out$importance[[1]][[1]]), "data.frame")
          })


# binary response Wed Feb  8 22:43:12 2017

test_that("sperrorest() produces correct output for binary response", {

  skip_on_cran()

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sperrorest(data = ecuador, formula = fo,
                    model_fun = glm, model_args = list(family = "binomial"),
                    pred_fun = predict, pred_args = list(type = "response"),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 2),
                    par_args = list(par_mode = "foreach", par_units = 2),
                    benchmark = TRUE,
                    importance = FALSE, imp_permutations = 2)
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
})

test_that("sperrorest() when pred_fun = NULL", {

  skip_on_cran()

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sperrorest(data = ecuador, formula = fo,
                    model_fun = glm, model_args = list(family = "binomial"),
                    pred_args = list(type = "response"),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 4),
                    par_args = list(par_mode = "foreach", par_units = 2),
                    benchmark = TRUE,
                    importance = TRUE, imp_permutations = 10)
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  summary_impo <- summary(out$importance)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
  # check for importance object
  expect_equal(class(out$importance[[1]][[1]]), "data.frame")
})

# par_mode = "future" Mon Feb  6 23:25:08 2017 ------------------------------


test_that("output type (= list) for different logical combinations of
          error_rep and error_fold for par_mode = 'future' on LDA example", {

            skip_on_cran()

            skip("par_mode = 'future' does not work on LDA example")

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
              if (!is.null(fac)) pred <- majority_filter(pred, newdata[,fac])
              return(pred)
            }

            fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
              b24 +
              b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
              b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
              b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
              b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
              ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
              ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
              ndwi08

            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda_predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:4, nfold = 2),
                              par_args = list(par_mode = "future",
                                              par_option = "cluster",
                                              par_units = 2),
                              benchmark = TRUE, progress = FALSE)

            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "list")
            # check for train_error existence
            expect_equal(names(out$error_rep)[[1]], "train_error")

          })

test_that("do.try argument", {

  skip_on_cran()

  skip("par_mode = 'future' does not work on LDA example")

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
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[,fac])
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

  # err.rep = TRUE, err.fold = TRUE
  out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                    model_fun = lda,
                    pred_fun = lda_predfun,
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 2),
                    par_args = list(par_mode = "future", par_units = 2),
                    error_rep = TRUE, error_fold = TRUE,
                    benchmark = TRUE, progress = FALSE,
                    do.try = T)

  expect_equal(typeof(out$error_rep), "list")
  expect_equal(typeof(out$error_fold), "list")
  # check for train_error existence
  expect_equal(names(out$error_rep)[[1]], "train_error")

})

test_that("output length of list is correct for par_mode = 'future' on rpart
          example", {

            skip_on_cran()

            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            # Example of a classification tree fitted to this data:
            mypred_rpart <- function(object, newdata) {
              predict(object, newdata)[, 2]
            }
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)

            out <- sperrorest(data = ecuador, formula = fo,
                              model_fun = rpart,
                              model_args = list(control = ctrl),
                              pred_fun = mypred_rpart,
                              progress = FALSE,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 2),
                              par_args = list(par_mode = "future",
                                              par_units = 2))

            expect_equal(length(out$error_fold[[1]]), 2)
          })

# par_mode = "apply" variable importance Tue Feb 21 22:15:41 2017 --------------

test_that("par_mode = 'apply' works with var.imp", {

  skip_on_cran()

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sperrorest(data = ecuador, formula = fo,
                    model_fun = glm, model_args = list(family = "binomial"),
                    pred_args = list(type = "response"),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 4),
                    par_args = list(par_mode = "apply", par_units = 2),
                    benchmark = TRUE,
                    importance = TRUE, imp_permutations = 2)
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  summary_impo <- summary(out$importance)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
  # check for importance object
  expect_type(out$importance[[1]][[1]], "list")
})

# sperrorest depr. args Thu Feb  9 22:42:48 2017

test_that("deprecated args", {

  skip_on_cran()

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

# partition_factor_cv mit custom pred_fun Sun Feb 19 09:36:26 2017

test_that("partition_factor_cv works (LDA)", {

  skip_on_cran()

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
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[,fac])
    return(pred)
  }

  data("maipo", package = "sperrorest")

  predictors <- colnames(maipo)[5:ncol(maipo)]
  # Construct a formula:
  fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))

  res_lda_sp_par <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                               model_fun = lda,
                               pred_fun = lda_predfun,
                               pred_args = list(fac = "field"),
                               smp_fun = partition_factor_cv,
                               smp_args = list(fac = "field", repetition = 1:2,
                                               nfold = 2),
                               par_args = list(par_units = 2,
                                               par_mode = "apply"),
                               error_rep = TRUE, error_fold = TRUE,
                               progress = "TRUE", benchmark = TRUE)

})

# par_mode = "sequential" Mon Feb  6 23:24:11 2017 --------------------------

test_that("output type (= list) for different logical combinations of error_rep
          and error_fold for par_mode = 'sequential' on LDA example", {

            skip_on_cran()

            lda_predfun <- function(object, newdata, fac = NULL) {
              library(nnet)
              majority <- function(x) {
                levels(x)[which.is.max(table(x))]
              }

              majority_filter <- function(x, fac) {
                for (lev in levels(fac)) {
                  x[fac == lev] <- majority(x[fac == lev])
                }
                x
              }

              pred <- predict(object, newdata = newdata)$class
              if (!is.null(fac)) pred <- majority_filter(pred, newdata[, fac])
              return(pred)
            }

            fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
              b24 +
              b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
              b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
              b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
              b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
              ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
              ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
              ndwi08

            data(maipo)

            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda_predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 4),
                              par_args = list(par_mode = "sequential"),
                              benchmark = TRUE, progress = T)

            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "list")
            # check that train_error is first
            expect_equal(names(out$error_rep)[[1]], "train_error")
          })


test_that("output length of list is correct for error_rep = TRUE and
          error_fold = TRUE for par_mode = 'sequential' on rpart example", {

            skip_on_cran()

            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            mypred_rpart <- function(object, newdata) predict(object,
                                                              newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)

            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred_rpart <- function(object, newdata) predict(object,
                                                              newdata)[,2]
            out <- sperrorest(data = ecuador, formula = fo,
                              model_fun = rpart,
                              model_args = list(control = ctrl),
                              pred_fun = mypred_rpart,
                              progress = FALSE,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2,
                                              nfold = 2),
                              par_args = list(par_mode = "sequential"),
                              error_rep = TRUE, error_fold = TRUE)

            expect_equal(length(out$error_fold[[1]]), 2)
          })

# variable importance Wed Feb  8 21:59:03 2017

test_that("sperrorest() variable importance with error_rep = T and
          error_fold = T", {

            skip_on_cran()

            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            out <- sperrorest(data = ecuador, formula = fo,
                              model_fun = glm,
                              model_args = list(family = "binomial"),
                              pred_fun = predict,
                              pred_args = list(type = "response"),
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 4),
                              par_args = list(par_mode = "sequential"),
                              benchmark = TRUE,
                              importance = TRUE, imp_permutations = 10)
            expect_equal(class(out$importance[[1]][[1]]), "data.frame")
          })

test_that("sperrorest() variable importance with error_rep = F and
          error_fold = T", {

            skip_on_cran()

            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

            out <- sperrorest(data = ecuador, formula = fo,
                              model_fun = glm,
                              model_args = list(family = "binomial"),
                              pred_fun = predict,
                              pred_args = list(type = "response"),
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 4),
                              par_args = list(par_mode = "sequential"),
                              benchmark = TRUE, error_rep = FALSE,
                              importance = TRUE, imp_permutations = 10)
            expect_equal(class(out$importance[[1]][[1]]), "data.frame")
          })

# binary response Wed Feb  8 22:43:12 2017

test_that("sperrorest() produces correct output for binary response", {

  skip_on_cran()

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sperrorest(data = ecuador, formula = fo,
                    model_fun = glm, model_args = list(family = "binomial"),
                    pred_fun = predict, pred_args = list(type = "response"),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 2),
                    par_args = list(par_mode = "sequential"),
                    benchmark = TRUE,
                    importance = FALSE, imp_permutations = 2)
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
})

test_that("sperrorest() when pred_fun = NULL", {

  skip_on_cran()

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sperrorest(data = ecuador, formula = fo,
                    model_fun = glm, model_args = list(family = "binomial"),
                    pred_args = list(type = "response"),
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 4),
                    par_args = list(par_mode = "sequential"),
                    benchmark = TRUE,
                    importance = TRUE, imp_permutations = 10)
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  summary_impo <- summary(out$importance)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
  # check for importance object
  expect_equal(class(out$importance[[1]][[1]]), "data.frame")
})

test_that("sperrorest() when missing factor levels in train data", {

  skip("internal use only")

  readRDS(paste0("/Users/pjs/Servers/GIServer/home/shares/data/LIFE/mod/",
                 "survey_data/data-clean.rda")) %>%
    as_tibble() %>%
    as.data.frame() -> df
  fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope + hail + age +
    ph + lithology + soil

  out <- sperrorest(data = df, formula = fo,
                    model_fun = glm, model_args = list(family = "binomial"),
                    pred_args = list(type = "response"),
                    smp_fun = partition_kmeans,
                    importance = TRUE, imp_permutations = 2,
                    smp_args = list(repetition = 1:2, nfold = 4),
                    par_args = list(par_mode = "sequential"))
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  summary_impo <- summary(out$importance)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
})

test_that("sperrorest() when missing factor levels in train data", {

  readRDS("/Users/pjs/Servers/GIServer/home/shares/data/LIFE/mod/survey_data/data-clean.rda") %>%
    as_tibble() -> df
  fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope + hail + age +
    ph + lithology + soil

  nspres <- sperrorest(data = df, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_args = list(type = "response"),
                       smp_fun = partition_kmeans,
                       smp_args = list(repetition = 1:2, nfold = 4),
                       par_args = list(par_mode = "sequential"))
  summary.rep <- summary(nspres$error_rep)
  summary.fold <- summary(nspres$error_fold)
  summary.resampling <- summary(nspres$represampling)
  summary.impo <- summary(nspres$importance)
  # check for train.auroc for binary response
  expect_equal(names(nspres$error_rep)[[1]], "train.auroc")
})
